#include <cstddef>
#include <cstdlib>
#include <cstring>
#include <dlfcn.h>
#include <emacs-module.h>
#include <filesystem>
#include <string>
#include <tree_sitter/api.h>

namespace fs = std::filesystem;

// typedef struct emacs_env_30 emacs_env;
typedef void *dynlib_handle_ptr;

// #define ATTRIBUTE_MAY_ALIAS __attribute__((__may_alias__))
// typedef void(ATTRIBUTE_MAY_ALIAS *dynlib_function_ptr)(void);

int plugin_is_GPL_compatible;

static void ts_lang_symbol_to_c_name(std::string &s) {
  for (char &c : s) {
    if (c == '-')
      c = '_';
  }
}

static void provide(emacs_env *env, const char *feature) {
  emacs_value Qfeat = env->intern(env, feature);
  emacs_value Qprovide = env->intern(env, "provide");
  emacs_value args[] = {Qfeat};

  env->funcall(env, Qprovide, 1, args);
}

/* Bind NAME to FUN.  */
static void bind_function(emacs_env *env, const char *name, emacs_value Sfun) {
  emacs_value Qdefalias = env->intern(env, "defalias");
  emacs_value Qsym = env->intern(env, name);
  emacs_value args[] = {Qsym, Sfun};

  env->funcall(env, Qdefalias, 2, args);
}

// static void ts_lang_message(emacs_env *env, const char *message) {
//   for (size_t i = 0; i < strlen(message); ++i) {
//     if (message[i] == '%')
//       return;
//   }
//   emacs_value data = env->make_string(env, message, strlen(message));
//   env->funcall(env, env->intern(env, "message"), 1, &data);
// }

static void ts_lang_error(emacs_env *env, const char *message) {
  emacs_value data = env->make_string(env, message, strlen(message));
  env->non_local_exit_signal(
      env, env->intern(env, "error"),
      env->funcall(env, env->intern(env, "list"), 1, &data));
}

// Load parser dynlib
// TODO(4/30/24): use Vtreesit_extra_load_path, or tree-sitter directory in
// user-emacs-directory.
static TSLanguage *ts_lang_load_language(emacs_env *env, const char *path) {
  const char *error;
  dynlib_handle_ptr handle = dlopen(path, RTLD_LAZY | RTLD_GLOBAL);
  error = dlerror();
  if (error != NULL) {
    ts_lang_error(env, error);
    return NULL;
  }

  fs::path p(path);
  auto lib_name = p.filename().stem().string();
  ts_lang_symbol_to_c_name(lib_name);

  TSLanguage *(*langfn)(void);
  langfn = (TSLanguage * (*)(void)) dlsym(handle, lib_name.substr(3).c_str());
  error = dlerror();
  if (error != NULL) {
    ts_lang_error(env, error);
    return NULL;
  }

  return (*langfn)();
}

// static int ts_lang_close(dynlib_handle_ptr h) { return dlclose(h) == 0; }

static ptrdiff_t string_bytes(emacs_env *env, emacs_value string) {
  ptrdiff_t size = 0;
  env->copy_string_contents(env, string, NULL, &size);
  return size;
}

static void ts_lang_free(void *arg) noexcept {
  TSLanguage *lang = (TSLanguage *)arg;
  ts_language_delete(lang);
}

emacs_value ts_lang_load(emacs_env *env, ptrdiff_t nargs, emacs_value args[],
                         void *data) noexcept {
  auto sz = string_bytes(env, args[0]);
  unsigned char p[sz];
  env->copy_string_contents(env, args[0], (char *)p, &sz);
  TSLanguage *lang = ts_lang_load_language(env, (char *)p);
  if (lang == NULL) {
    return env->intern(env, "nil");
  }
  return env->make_user_ptr(env, ts_lang_free, lang);
}

// Returns '((named nodes...) (anon nodes...) (fields...))
emacs_value ts_lang_parser_info(emacs_env *env, ptrdiff_t nargs,
                                  emacs_value args[], void *data) noexcept {
  TSLanguage *lang = (TSLanguage *)env->get_user_ptr(env, args[0]);
  if (lang == NULL) {
    return env->intern(env, "nil");
  }

  // Language symbols
  uint32_t nsymbols = ts_language_symbol_count(lang);
  emacs_value named[nsymbols];
  emacs_value anon[nsymbols];
  size_t named_count = 0, anon_count = 0;
  for (size_t i = 0; i < nsymbols; ++i) {
    TSSymbolType t = ts_language_symbol_type(lang, (TSSymbol)i);
    // Hidden noes never returned from API
    if (t == TSSymbolTypeAuxiliary) {
      continue;
    }

    auto sym = ts_language_symbol_name(lang, (TSSymbol)i);
    if (!sym)
      continue;

    if (t == TSSymbolTypeRegular) {
      named[named_count++] = env->make_string(env, sym, strlen(sym));
    } else {
      anon[anon_count++] = env->make_string(env, sym, strlen(sym));
    }
  }

  // Language fields
  // Note: ids start at 1
  uint32_t nfields = ts_language_field_count(lang);
  emacs_value fields[nfields];
  size_t field_count = 0;
  for (size_t i = 1; i <= nfields; ++i) {
    auto field = ts_language_field_name_for_id(lang, (TSFieldId)i);
    if (field)
      fields[field_count++] = env->make_string(env, field, strlen(field));
  }

  emacs_value lists[3] = {
      env->funcall(env, env->intern(env, "list"), named_count, named),
      env->funcall(env, env->intern(env, "list"), anon_count, anon),
      env->funcall(env, env->intern(env, "list"), field_count, fields),
  };
  return env->funcall(env, env->intern(env, "list"), 3, lists);
}

int emacs_module_init(struct emacs_runtime *runtime) {
  if (runtime->size < (ptrdiff_t)sizeof(*runtime))
    return 1;

  emacs_env *env = runtime->get_environment(runtime);

  // Register symbols / functions
#define DEFUN(lsym, csym, amin, amax, doc, data)                               \
  bind_function(env, lsym, env->make_function(env, amin, amax, csym, doc, data))

  DEFUN("ts-lang-load", ts_lang_load, 1, 1, "Load language from library", NULL);
  DEFUN("ts-lang--parser-info", ts_lang_parser_info, 1, 1,
        "Get symbols and fields defined by language parser", NULL);

#undef DEFUN

  provide(env, "ts-lang-module");
  return 0;
}

// extern "C" {
// #include "javascript/src/parser.c"
// #include "javascript/src/scanner.c"
// }

// #define LANGUAGE tree_sitter_javascript
// #define SOURCE_PATH "javascript/examples/jquery.js"

// int main() {
//   TSParser *parser = ts_parser_new();
//   if (!ts_parser_set_language(parser, LANGUAGE())) {
//     fprintf(stderr, "Invalid language\n");
//     exit(1);
//   }

//   const char *source_path = GRAMMARS_DIR SOURCE_PATH;

//   printf("Parsing %s\n", source_path);

//   std::ifstream source_file(source_path);
//   if (!source_file.good()) {
//     fprintf(stderr, "Invalid source path %s\n", source_path);
//     exit(1);
//   }

//   std::string source_code((std::istreambuf_iterator<char>(source_file)),
//                           std::istreambuf_iterator<char>());

//   TSTree *tree = ts_parser_parse_string(parser, NULL, source_code.c_str(),
//                                         source_code.size());
// }
