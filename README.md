# My Emacs

### Setup

```sh
$ mkdir ~/emacs
$ git clone git@github.com:govi218/.emacs.d.git ~/emacs/emacs-gov
$ ln -s ~/emacs/emacs-gov ~/.emacs.d
```
### Requirements

- emacs >= 27

### Installing
- For MacOS, it's highly recommended to use [Emacs For MacOS](https://emacsformacosx.com/) instead of other ones
- For Linux, refer to your appropriate package manager
- There are a few more packages that need to be installed which you'll see in the warning/error buffer (and that I'll document here someday™)

### Editing
This is an evil editor (vim bindings). This setup is configured mostly for javascript/typescript, Go, C++, python, sql, and haskell. It attempts to use the Language Server Protocol when possible to give an IDE-like completion experience.

### C++ Mode
This conf uses [ccls](https://github.com/MaskRay/ccls) for formatting. For best results, generate a `compile_commands.json` file with cmake for your project. This can be done using `cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 ..`. Then you can move this file to the project root and gitignore it. This allows CCLS to find and index all the deps for the project.

### JS/TS Mode
- This conf uses [tide](https://github.com/ananthakumaran/tide). A prettier conf is "required", i.e. you'll keep getting warnings on save if you don't have it when you save. To kill this, disable `prettier-format-on-save` in `my-javascript.el`
- If a JS/TS file isn't recognized as such, `web-mode` should get you back

### AI
This conf uses [emigo](https://github.com/MatthewZMD/emigo). Make sure you have a `.env` file in the project root with `OPENROUTER_API_KEY` set (of course, using [OpenRouter](https://openrouter.ai)).
