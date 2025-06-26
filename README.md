# poly-translate

Multi-backend translation library for Emacs with support for Google Translate, DeepL, and LLM-based translation services.

## Features

- **Multi-backend support**: Google Translate API, DeepL API, and LLM-based services (OpenAI, Anthropic Claude, Gemini)
- **Plugin architecture**: Easy to add new translation backends
- **Flexible translation**: Translate selected regions or entire buffers
- **Auto-detection**: Automatic source language detection
- **Caching**: Translation results are cached to reduce API calls
- **Rate limiting**: Built-in rate limiting to respect API quotas
- **Rich UI**: Dedicated buffer for translation results with interactive commands
- **Kill ring integration**: Copy translations directly to Emacs kill ring

## Requirements

- Emacs 30.1 or later
- [gptel](https://github.com/karthink/gptel) (for LLM backends)

## Installation

1. Clone this repository:
   ```bash
   git clone https://github.com/suzuki/poly-translate.git
   ```

2. Add to your Emacs configuration:
   ```elisp
   (add-to-list 'load-path "/path/to/poly-translate")
   (add-to-list 'load-path "/path/to/poly-translate/backends")
   (require 'poly-translate)
   ```

3. Install gptel if you want to use LLM backends:
   ```elisp
   (package-install 'gptel)
   ```

## Quick Start

1. **Configure a translation engine**:
   ```elisp
   ;; Google Translate
   (poly-translate-register-google-engine
    "Google Auto to Japanese" "auto" "ja" "YOUR_GOOGLE_API_KEY")

   ;; DeepL
   (poly-translate-register-deepl-engine
    "DeepL Auto to English" "auto" "en" "YOUR_DEEPL_API_KEY")

   ;; OpenAI (requires gptel)
   (poly-translate-register-llm-engine
    "GPT-4 Auto to Japanese" 'openai "auto" "ja" "YOUR_OPENAI_API_KEY")
   ```

2. **Use translation commands**:
   - `M-x poly-translate-region` - Translate selected text
   - `M-x poly-translate-buffer` - Translate entire buffer
   - `M-x poly-translate-region-to-kill-ring` - Translate and copy to kill ring

## Configuration

See [examples/poly-translate-config.el](examples/poly-translate-config.el) for comprehensive configuration examples.

### API Keys

**Important**: Never commit API keys to version control. Use one of these methods:

1. **Environment variables**:
   ```elisp
   (setq poly-translate-google-api-key (getenv "GOOGLE_TRANSLATE_API_KEY"))
   ```

2. **Private configuration file**:
   ```elisp
   ;; In ~/.emacs.d/private.el
   (setq poly-translate-deepl-api-key "your-api-key-here")
   ```

3. **auth-source**:
   ```elisp
   (setq poly-translate-openai-api-key
         (auth-source-pick-first-password :host "api.openai.com"))
   ```

### Key Bindings

```elisp
(global-set-key (kbd "C-c t r") #'poly-translate-region)
(global-set-key (kbd "C-c t b") #'poly-translate-buffer)
(global-set-key (kbd "C-c t y") #'poly-translate-region-to-kill-ring)
```

## Supported Backends

### Google Translate
- Requires: Google Cloud Translation API key
- Features: Auto-detection, 100+ languages
- Rate limit: 100 requests/hour (configurable)

### DeepL
- Requires: DeepL API key (free or pro)
- Features: High-quality translations, 30+ languages
- Rate limit: Based on your DeepL plan

### LLM Backends (via gptel)
- **OpenAI**: GPT-4, GPT-3.5 Turbo
- **Anthropic**: Claude 3 (Opus, Sonnet, Haiku)
- **Google**: Gemini Pro
- Features: Context-aware translation, multiple styles (formal, casual, technical, literary)

## Translation Styles (LLM only)

- `default`: Standard translation
- `formal`: Formal tone
- `casual`: Conversational tone
- `technical`: Preserves technical terminology
- `literary`: Maintains literary style and nuance

## Development

### Quick Start with Make

```bash
# Run all tests (including native compilation check)
make test

# Run only unit tests
make test-unit

# Test native compilation (if available)
make test-native-compile

# Compile all files
make compile

# Clean compiled files (including native compiled)
make clean

# See all available commands
make help
```

### Manual Commands

#### Byte Compilation

```bash
# Compile all files
emacs -Q -batch -L . -L backends -f batch-byte-compile poly-translate.el poly-translate-*.el backends/*.el

# Compile single file
emacs -Q -batch -L . -L backends -f batch-byte-compile poly-translate.el
```

#### Running Tests

```bash
# Run all tests
emacs -Q -batch -L . -L tests -l ert -l tests/poly-translate-test.el -f ert-run-tests-batch-and-exit

# Run specific test
emacs -Q -batch -L . -L tests -l ert -l tests/poly-translate-test.el --eval "(ert-run-tests-batch-and-exit 'poly-translate-test-engine-registration)"

# Using make for specific test
make test-one TEST=poly-translate-test-engine-registration
```

### Adding New Backends

1. Create a new file in `backends/` directory (e.g., `poly-translate-my-backend.el`)
2. Define the backend symbol and implement required functions:
   ```elisp
   (defconst poly-translate-backend-my-backend 'my-backend
     "Symbol for my custom backend.")
   
   (defun poly-translate-backend-my-backend-translate (text from-lang to-lang config callback error-callback)
     "Translate TEXT from FROM-LANG to TO-LANG using my backend."
     ;; Implementation here
     )
   
   (defun poly-translate-backend-my-backend-validate-config (config)
     "Validate CONFIG for my backend."
     ;; Return t if valid, nil otherwise
     )
   ```
3. Register the backend:
   ```elisp
   (poly-translate-register-backend 
    'my-backend
    `(:translate ,#'poly-translate-backend-my-backend-translate
      :validate-config ,#'poly-translate-backend-my-backend-validate-config))
   ```
4. Add convenience function for engine registration:
   ```elisp
   (defun poly-translate-register-my-backend-engine (name from-lang to-lang api-key)
     "Register a my-backend translation engine."
     (poly-translate-register-engine
      `(:name ,name
        :backend my-backend
        :input-lang ,from-lang
        :output-lang ,to-lang
        :api-key ,api-key)))
   ```

## License

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

## Contributing

Contributions are welcome! Please ensure:
- Code follows Emacs Lisp conventions
- All tests pass
- New features include tests
- Documentation is updated

## Troubleshooting

### Common Issues

1. **"gptel is not available"**: Install gptel package for LLM backends
2. **"API key not configured"**: Set up API keys as described above
3. **"Rate limit exceeded"**: Wait or configure rate limits
4. **"No translation found"**: Check API key and network connection

### Debug Mode

Enable debug mode for troubleshooting:
```elisp
(setq poly-translate-debug t)
```

## Acknowledgments

- [gptel](https://github.com/karthink/gptel) for LLM integration
- [go-translate](https://github.com/lorniu/go-translate) for inspiration
- [emacs-immersive-translate](https://github.com/Elilif/emacs-immersive-translate) for reference implementation
