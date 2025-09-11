# BabyVim

A tiny terminal text editor written in C. Based on the tutorial https://viewsourcecode.org/snaptoken/kilo
No `ncurses`, only raw `termios` + ANSI escapes.
Modes like Vim. Minimal, hackable, fast.

---

## Features

* **Two modes:** `NORMAL` and `INSERT`.
* **Soft/Hard/No wrap:** `BABYVIM_WRAP_LINES` = `2 | 1 | 0`.
* **Line numbers sidebar** with dynamic width.
* **Syntax highlighting** for C and Python (strings, numbers, comments, keywords).
* **Search** (`Ctrl-F`) with live highlight and next/prev via arrows.
* **Selection** with Shift + arrows. Copy (`Ctrl-C`), Cut (`Ctrl-X`), Paste (`Ctrl-V`).
* **Indent-aware Home/End.** Tabs as spaces (configurable).
* **“Hippie completion”**: type word, press `` ` `` to complete from later occurrences.
* **Command prompt** (`:`) with simple commands (`:w`, `:q`, `:wq`, `:x`, line jumps, etc.).
* **Hard wrap pass** on file open when enabled.
* **Single-file build.** No external deps.

---

## Build

Requires a POSIX-like system (Linux/macOS) and a C compiler.

```bash
make
```

Run:

```bash
./babyvim [path/to/file] [ +N | -N ]
```

* `+N` opens in **INSERT** mode at line `N`.
* `-N` opens in **NORMAL** mode at line `N`.

---

## Config (edit `#define`s)

| Define               | Meaning                      | Default |
| -------------------- | ---------------------------- | ------- |
| `BABYVIM_TAB_STOP`   | Tab width                    | `4`     |
| `BABYVIM_SPACE_TYPE` | `1` spaces, `0` real tabs    | `1`     |
| `BABYVIM_WRAP_LINES` | `0` none, `1` hard, `2` soft | `2`     |
| `BABYVIM_QUIT_TIMES` | Quit confirmation count      | `3`     |

Syntax DB lives in `HLDB` (C & Python included). Add your own filetypes there.

---

## Keybindings

### Global

| Keys                      | Action                                   |
| ------------------------- | ---------------------------------------- |
| `Ctrl-S`                  | Save                                     |
| `Ctrl-Q`                  | Quit (with confirmation if modified)     |
| `Ctrl-F`                  | Search (type, use arrows, `Enter`/`Esc`) |
| `PageUp/PageDown`         | Page scroll                              |
| `Home/End`                | Jump line start (indent-aware) / end     |
| `Shift + Arrows/Home/End` | Start/extend selection                   |

### Modes

**NORMAL**

| Keys         | Action                        |
| ------------ | ----------------------------- |
| `i`          | Enter INSERT mode             |
| `:`          | Command prompt                |
| *(movement)* | Arrows, PageUp/Down, Home/End |

**INSERT**

| Keys                      | Action                               |
| ------------------------- | ------------------------------------ |
| `Esc` or `Ctrl-L`         | Back to NORMAL                       |
| `Enter`                   | New line (keeps indent)              |
| `Tab`                     | Insert spaces (or tab if configured) |
| `` ` ``                   | Hippie completion (end of word)      |
| `Backspace/Delete/Ctrl-H` | Delete (indent-aware backspace)      |
| `Ctrl-C`                  | Copy selection                       |
| `Ctrl-X`                  | Cut selection                        |
| `Ctrl-V`                  | Paste                                |

---

## Commands (`:` in NORMAL)

* `:w [file]` — Save (optionally as `file`).
* `:q` or `:q!` — Quit.
* `:wq` or `:x` — Save & quit.
* `:N` or `:line N` or `:l N` — Go to line `N`.
* `:$` — Go to last visual line.
* `:.` — Go to top of screen.
* `:^` — Go to first visual line on screen.
* `:set number|nonumber|wrap|nowrap` — Placeholders for future toggles.

Tip: You can also pass `+N`/`-N` on CLI to jump to a line and pick the mode.

---

## Selection, Copy/Cut/Paste

* Hold **Shift** with arrows/Home/End to select.
* `Ctrl-C` to copy. `Ctrl-X` to cut. `Ctrl-V` to paste.
* Works across multiple lines. Preserves tail content correctly.

---

## Wrapping

* **Soft wrap (`2`)**: Screen-wrapped drawing with correct cursor math (subrow-aware).
* **Hard wrap (`1`)**: On-the-fly splitting at word boundaries; also a wrap pass after file open.
* **No wrap (`0`)**: Horizontal scrolling.

---

## Syntax Highlighting

* C and Python:

  * Comments (line + multi-line), strings, numbers, keywords (two classes).
* Add more in `HLDB[]`:

  * Set `filematch`, `keywords`, comment delimiters, and `flags`.

---

## Search

* `Ctrl-F` opens prompt.
* Type query; use arrows to jump to next/prev match.
* `Enter` keeps the last match; `Esc` cancels.

---

## Hippie Completion

* In INSERT, at the **end of a word**, press `` ` ``.
* Looks ahead in the buffer for the next word sharing the same prefix.
* Inserts only the remaining suffix.

---

## File I/O

* Open existing or new files.
* Saves with `O_CREAT` (0644) and `ftruncate`.
* Tracks dirty state; warns on quit.

---

## Known Limitations

* Terminal must support ANSI escapes.
* No Unicode grapheme awareness (operates on bytes).
* Limited filetype DB.
* Command `:set` options are placeholders (no runtime toggles yet).

---

## Roadmap

* More filetypes & themes.
* Config files.
* Persistent settings and live `:set` toggles.
* Search/replace, undo/redo.
* Visual block mode.

---

## Contributing

Issues and PRs welcome. Keep it single-file and readable.
Stick to C99, avoid extra deps, prefer small helpers over macros.

---

## License

Do what you want. Just don’t blame me if it eats your homework.
