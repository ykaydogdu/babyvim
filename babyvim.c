/*** includes ***/

#define _DEFAULT_SOURCE
#define _BSD_SOURCE
#define _GNU_SOURCE

#include <ctype.h>
#include <stdarg.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <termios.h>
#include <time.h>
#include <unistd.h>

/*** defines ***/

#define BABYVIM_VERSION "0.0.1"
#define BABYVIM_SPACE_TYPE 1 // 0: tabs, 1: spaces
#define BABYVIM_TAB_STOP 4
#define BABYVIM_QUIT_TIMES 3
#define BABYVIM_WRAP_LINES 2 // 0: no wrap, 1: hard wrap, 2: soft wrap

#define CTRL_KEY(k) ((k) & 0x1f)
#define is_space(c) (c == ' ' || c == '\t')

enum editorKey {
    BACKSPACE = 127,
    ARROW_LEFT = 1000,
    ARROW_RIGHT,
    ARROW_UP,
    ARROW_DOWN,
    DEL_KEY,
    HOME_KEY,
    END_KEY,
    PAGE_UP,
    PAGE_DOWN
};
#define SHIFT_KEY(k) (k + 1000) // arbitrary offset to editorKey enum
#define isShiftKey(k) (k >= 2000)
#define translateShiftKey(k) (isShiftKey(k) ? k - 1000 : k)

enum editorHighlight {
    HL_NORMAL = 0,
    HL_COMMENT,
    HL_MLCOMMENT,
    HL_KEYWORD1,
    HL_KEYWORD2,
    HL_STRING,
    HL_NUMBER,
    HL_MATCH,
};

#define HL_HIGHLIGHT_NUMBERS (1<<0)
#define HL_HIGHLIGHT_STRINGS (1<<1)

enum editorMode {
    NORMAL_MODE,
    INSERT_MODE
};

/*** data ***/

struct editorSyntax {
    char *filetype;
    char **filematch;
    char **keywords;
    char *singleline_comment_start;
    char *multiline_comment_start;
    char *multiline_comment_end;
    int flags;
};

typedef struct erow {
    int idx;
    int size;
    int rsize;
    int indent;
    char *chars;
    char *render;
    unsigned char *hl;
    int hl_open_comment;
} erow;

struct editorSelection {
    int active;
    int sx, sy;
};

#define EDITOR_SELECTION_INIT {0, -1, -1}

struct editorConfig {
    enum editorMode mode;
    int commandEntering;
    int cx, cy;
    int rx;
    int rowoff;
    int coloff;
    int screenrows;
    int screencols;
    int textrows;
    int textcols;
    int numrows;
    int linenumwidth;
    erow *row;
    int dirty;
    char *filename;
    char statusmsg[80];
    time_t statusmsg_time;
    char *copyBuffer;
    struct editorSelection selection;
    struct editorSyntax *syntax;
    struct termios orig_termios;
};

struct editorConfig E;

/*** filetypes ***/

char *C_HL_extensions[] = {".c", ".h", ".cpp", NULL};
char *C_HL_keywords[] = {
    "switch", "if", "while", "for", "break", "continue", "return", "else",
    "struct", "union", "typedef", "static", "enum", "class", "case",
    "int|", "long|", "double|", "float|", "char|", "unsigned|", "signed|",
    "void|", NULL
  };

char *PY_HL_extensions[] = {".py", NULL};
char *PY_HL_keywords[] = {
    "if", "else", "elif", "while", "for", "break", "continue", "return", "def",
    "class", "import", "from", "as", "is", "in", "and", "or", "not", "with",
    "yield", "lambda", "global", "nonlocal", "raise", "try", "except", "finally",
    "print|", "input|", "open|", "close|", "read|", "write|", "append|", "seek|", "tell|",
    "flush|", "truncate|", "copy|", "move|", "rename|", "delete|", "exists|", "list|",
    "dict|", "set|", "tuple|", NULL
};

struct editorSyntax HLDB[] = {
    {
        "c",
        C_HL_extensions,
        C_HL_keywords,
        "//", "/*", "*/",
        HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS,
    },
    {
        "python",
        PY_HL_extensions,
        PY_HL_keywords,
        "#", "'''", "'''",
        HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS,
    },
};

#define HLDB_ENTRIES (sizeof(HLDB) / sizeof(HLDB[0]))

/*** prototypes ***/

void editorSetStatusMessage(int clear, const char *fmt, ...);
void editorRefreshScreen();
char *editorPrompt(char *prompt, void (*callback)(char *, int));
void editorHardWrapRow(int idx);
void editorIndentRow(erow *row, int indent);

static int WRAP_IN_PROGRESS = 0;
static int FILE_LOADING = 0;

/*** append buffer ***/

struct abuf {
    char *b;
    int len;
};

#define ABUF_INIT {NULL, 0}

void abAppend(struct abuf *ab, const char *s, int len) {
    char *new = realloc(ab->b, ab->len + len);

    if (new == NULL) return;
    memcpy(&new[ab->len], s, len);
    ab->b = new;
    ab->len += len;
}

void abFree(struct abuf *ab) {
    free(ab->b);
}

/*** terminal ***/

void die(const char *s) {
    write(STDOUT_FILENO, "\x1b[2J", 4); // Clear screen
    write(STDOUT_FILENO, "\x1b[H", 3); // Move cursor to top-left corner
    
    perror(s);
    exit(1);
}

void disableRawMode() {
    if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &E.orig_termios) == -1) die("tcsetattr");
}

void enableRawMode() {
    printf("\x1b[?2004l"); // disable bracketed paste
    fflush(stdout);

    if (tcgetattr(STDIN_FILENO, &E.orig_termios) == -1) die("tcgetattr");
    atexit(disableRawMode);

    struct termios raw = E.orig_termios;
    raw.c_iflag &= ~(ICRNL | IXON | BRKINT | INPCK | ISTRIP); // Disable software flow control (Ctrl-S, Ctrl-Q)
    raw.c_oflag &= ~(OPOST); // Disable output processing
    raw.c_cflag |= (CS8); // Set 8 bits per byte
    raw.c_lflag &= ~(ECHO | ICANON | ISIG | IEXTEN); // Disable echo, canonical mode, and signal interrupts
    raw.c_cc[VMIN] = 0; // Minimum number of characters to read
    raw.c_cc[VTIME] = 1; // Timeout in tenths of a second

    if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw) == -1) die("tcsetattr");
}

int editorReadKey() {
    int nread;
    char c;
    while ((nread = read(STDIN_FILENO, &c, 1)) != 1) {
        if (nread == -1 && errno != EAGAIN) die("read");
    }

    // Handle escape sequences
    if (c == '\x1b') {
        char seq[5];

        if (read(STDIN_FILENO, &seq[0], 1) != 1) return '\x1b';
        if (read(STDIN_FILENO, &seq[1], 1) != 1) return '\x1b';
        
        if (seq[0] == '[') {
            if (seq[1] >= '0' && seq[1] <= '9') {
                if (read(STDIN_FILENO, &seq[2], 1) != 1) return '\x1b';
                if (seq[2] == '~') {
                    switch (seq[1]) {
                        case '1': return HOME_KEY;
                        case '3': return DEL_KEY;
                        case '4': return END_KEY;
                        case '5': return PAGE_UP;
                        case '6': return PAGE_DOWN;
                        case '7': return HOME_KEY;
                        case '8': return END_KEY;
                    }
                } else if (seq[2] == ';') { // combination keys
                    if (read(STDIN_FILENO, &seq[3], 1) != 1) return '\x1b';
                    if (seq[3] == '2') { // shift keys
                        if (read(STDIN_FILENO, &seq[4], 1) != 1) return '\x1b';
                        switch (seq[4]) {
                            case 'A': return SHIFT_KEY(ARROW_UP);
                            case 'B': return SHIFT_KEY(ARROW_DOWN);
                            case 'C': return SHIFT_KEY(ARROW_RIGHT);
                            case 'D': return SHIFT_KEY(ARROW_LEFT);
                            case 'H': return SHIFT_KEY(HOME_KEY);
                            case 'F': return SHIFT_KEY(END_KEY);
                        }
                    }
                }
            }
            switch (seq[1]) {
                case 'A': return ARROW_UP;
                case 'B': return ARROW_DOWN;
                case 'C': return ARROW_RIGHT;
                case 'D': return ARROW_LEFT;
                case 'H': return HOME_KEY;
                case 'F': return END_KEY;
            }
        } else if (seq[0] == 'O') {
            switch (seq[1]) {
                case 'H': return HOME_KEY;
                case 'F': return END_KEY;
            }
        }

        return '\x1b';
    }
    return c;
}

/*** row operations ***/

// visual height of a row in wrapped mode
static inline int rowVisualHeight(erow *row) {
    if (BABYVIM_WRAP_LINES != 2) return 1;
    if (!row) return 1;
    return row->rsize == 0 ? 1 : (row->rsize - 1) / E.textcols + 1;
}

// which subrow is the cursor inside this row
static inline int rowSubrowAtRx(erow *row, int rx) {
    if (BABYVIM_WRAP_LINES != 2) return rx;
    if (!row) return rx;
    return rx / E.textcols;
}

// rx inside the current subrow
static inline int rowcolInSubrow(erow *row, int rx) {
    if (BABYVIM_WRAP_LINES != 2) return rx;
    if (!row) return rx;
    return rx % E.textcols;
}

// absolute visual row index of (filerow = r, subrow = s)
static inline int rowAbsoluteVisualRow(int r, int s) {
    if (BABYVIM_WRAP_LINES != 2) return r;
    int v = 0;
    for (int i = 0; i < r; i++) v += rowVisualHeight(&E.row[i]);
    return v + s;
}

static void visualIndexToFile(int vindex, int *outRow, int *outSubrow) {
    if (BABYVIM_WRAP_LINES != 2) {
        *outRow = vindex;
        *outSubrow = 0;
        return;
    }
    int acc = 0;
    for (int r = 0; r < E.numrows; r++) {
        int h = rowVisualHeight(&E.row[r]);
        if (acc + h > vindex) {
            *outRow = r;
            *outSubrow = vindex - acc;
            return;
        }
        acc += h;
    }
    *outRow = E.numrows > 0 ? E.numrows - 1 : 0; // clamp to last row
    *outSubrow = 0;
}

static inline int isLessCoord(int x1, int y1, int x2, int y2) {
    return y1 < y2 || (y1 == y2 && x1 < x2);
}

void refreshCursorPosition(struct abuf *ab) {
    if (E.commandEntering) {
        char buf[32];
        int len = snprintf(buf, sizeof(buf), "\x1b[%d;%luH", E.screenrows, strlen(E.statusmsg) + 1);
        abAppend(ab, buf, len);
        return;
    }

    int vrow, vcol;
    erow *row = E.cy < E.numrows ? &E.row[E.cy] : NULL;

    if (BABYVIM_WRAP_LINES == 2 && row) {
        int rx = E.rx;
        int sub = rowSubrowAtRx(row, rx);
        vrow = rowAbsoluteVisualRow(E.cy, sub);
        vcol = rowcolInSubrow(row, rx);
    } else {
        vrow = E.cy;
        vcol = E.rx;
    }

    // final on screen pos
    int scr_y = vrow - E.rowoff + 1;
    int scr_x = vcol - E.coloff + E.linenumwidth + 1;

    char buf[32];
    snprintf(buf, sizeof(buf), "\x1b[%d;%dH", scr_y, scr_x);
    abAppend(ab, buf, strlen(buf));
}

int getCursorPosition(int *rows, int *cols) {
    char buf[32];
    unsigned int i = 0;

    if (write(STDOUT_FILENO, "\x1b[6n", 4) != 4) return -1;
    
    while (i < sizeof(buf) - 1) {
        if (read(STDIN_FILENO, &buf[i], 1) != 1) break;
        if (buf[i] == 'R') break;
        i++;
    }
    buf[i] = '\0';
    
    if (buf[0] != '\x1b' || buf[1] != '[') return -1;
    if (sscanf(&buf[2], "%d;%d", rows, cols) != 2) return -1;
    return 0;
}

int getWindowSize(int *rows, int *cols) {
    struct winsize ws;

    if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == -1 || ws.ws_col == 0) {
        if (write(STDOUT_FILENO, "\x1b[999C\x1b[999B", 12) != 12) return -1;
        return getCursorPosition(rows, cols);
    } else {
        *cols = ws.ws_col;
        *rows = ws.ws_row;
        return 0;
    }
}

/*** syntax highlighting ***/

int is_seperator(int c) {
    return isspace(c) || c == '\0' || strchr(",.()+-/*=~%<>[];", c) != NULL;
}

void editorUpdateSyntax(erow *row) {
    row->hl = realloc(row->hl, row->rsize);
    memset(row->hl, HL_NORMAL, row->rsize);

    if (E.syntax == NULL) return;

    char **keywords = E.syntax->keywords;

    // singleline comment start
    char *scs = E.syntax->singleline_comment_start;
    char *mcs = E.syntax->multiline_comment_start;
    char *mce = E.syntax->multiline_comment_end;

    int scs_len = scs ? strlen(scs) : 0;
    int mcs_len = mcs ? strlen(mcs) : 0;
    int mce_len = mce ? strlen(mce) : 0;

    // previous separator
    int prev_sep = 1;
    // in string flag
    int in_string = 0;
    // in multiline comment flag
    int in_comment = (row->idx > 0 && E.row[row->idx - 1].hl_open_comment);

    int i = 0;
    while (i < row->rsize) {
        char c = row->render[i];
        unsigned char prev_hl = (i > 0) ? row->hl[i - 1] : HL_NORMAL;

        // singleline comment highlighting
        if (scs_len && !in_string && !in_comment) {
            if (!strncmp(&row->render[i], scs, scs_len)) {
                memset(&row->hl[i], HL_COMMENT, row->rsize - i);
                break;
            }
        }

        // multiline comment highlighting
        if (mcs_len && mce_len && !in_string) {
            if (in_comment) {
                row->hl[i] = HL_MLCOMMENT;
                if (!strncmp(&row->render[i], mce, mce_len)) {
                    memset(&row->hl[i], HL_MLCOMMENT, mce_len);
                    in_comment = 0;
                    prev_sep = 1;
                    continue;
                } else {
                    i++;
                    continue;
                }
            } else if (!strncmp(&row->render[i], mcs, mcs_len)) {
                memset(&row->hl[i], HL_MLCOMMENT, mcs_len);
                in_comment = 1;
                i += mcs_len;
                continue;
            }
        }

        // string highlighting
        if (E.syntax->flags & HL_HIGHLIGHT_STRINGS) {
            if (in_string) { // in string
                row->hl[i] = HL_STRING;
                if (c == '\\' && i + 1 < row->rsize) { // escape characters do not close the string
                    row->hl[i + 1] = HL_STRING;
                    i += 2;
                    continue;
                }
                if (c == in_string) in_string = 0; // end of string
                i++;
                prev_sep = 1;
                continue;
            } else {
                if (c == '"' || c == '\'') { // start of string
                    in_string = c;
                    row->hl[i] = HL_STRING;
                    i++;
                    prev_sep = 1;
                    continue;
                }
            }
        }

        if (E.syntax->flags & HL_HIGHLIGHT_NUMBERS) {
            if ((isdigit(c) && (prev_sep || prev_hl == HL_NUMBER)) ||
                (c == '.' && prev_hl == HL_NUMBER)) { // decimal point
                row->hl[i] = HL_NUMBER;
                i++;
                prev_sep = 0;
                continue;
            }
        }

        if (prev_sep) {
            int j = 0;
            for (j = 0; keywords[j]; j++) {
                int klen = strlen(keywords[j]);
                int kw2 = keywords[j][klen - 1] == '|';
                if (kw2) klen--;

                if (!strncmp(&row->render[i], keywords[j], klen) &&
                    is_seperator(row->render[i + klen])) {
                    memset(&row->hl[i], kw2 ? HL_KEYWORD2 : HL_KEYWORD1, klen);
                    i += klen;
                    break;
                }
            }
            if (keywords[j] != NULL) {
                prev_sep = 0;
                continue;
            }
        }

        prev_sep = is_seperator(c);
        i++;
    }

    int changed = (row->hl_open_comment != in_comment);
    row->hl_open_comment = in_comment;
    if (changed && row->idx + 1 < E.numrows) {
        editorUpdateSyntax(&E.row[row->idx + 1]); // propagate the change to the next row
    }
}

int editorSyntaxToColor(int hl) {
    switch (hl) {
        case HL_COMMENT:
        case HL_MLCOMMENT: return 36;
        case HL_KEYWORD1: return 33;
        case HL_KEYWORD2: return 32;
        case HL_STRING: return 35;
        case HL_NUMBER: return 31;
        case HL_MATCH: return 34;
        default: return 37;
    }
}

void editorSelectSyntaxHighlight() {
    E.syntax = NULL;
    if (E.filename == NULL) return;

    char *ext = strrchr(E.filename, '.');
    for (unsigned int j = 0; j < HLDB_ENTRIES; j++) {
        struct editorSyntax *s = &HLDB[j];
        unsigned int i = 0;
        while (s->filematch[i]) {
            int is_ext = s->filematch[i][0] == '.';
            if ((is_ext && ext && !strcmp(ext, s->filematch[i])) ||
                (!is_ext && strstr(E.filename, s->filematch[i]))) {
                E.syntax = s;

                for (int filerow = 0; filerow < E.numrows; filerow++) {
                    editorUpdateSyntax(&E.row[filerow]);
                }
                return;
            }
            i++;
        }
    }
}

/*** row operations ***/

int editorRowCxToRx(erow *row, int cx) {
    int rx = 0;
    for (int j = 0; j < cx; j++) {
        if (row->chars[j] == '\t') rx += (BABYVIM_TAB_STOP - 1) - (rx % BABYVIM_TAB_STOP);
        rx++;
    }
    return rx;
}

int editorRowRxToCx(erow *row, int rx) {
    int cur_rx = 0;
    int cx = 0;
    for (cx = 0; cx < row->size; cx++) {
        if (row->chars[cx] == '\t')
            cur_rx += (BABYVIM_TAB_STOP - 1) - (cur_rx % BABYVIM_TAB_STOP);
        cur_rx++;

        if (cur_rx > rx) break;
    } 
    return cx;
}

void editorUpdateRow(erow *row) {
    int j;

    int indent = 0;
    int tabs = 0;
    for (j = 0; j < row->size; j++) {
        if (row->chars[j] == '\t') {
            tabs++;
            indent += BABYVIM_TAB_STOP;
        } else if (row->chars[j] == ' ') indent++;
        else break;
    }
    row->indent = indent;

    for (; j < row->size; j++)
        if (row->chars[j] == '\t') tabs++;

    free(row->render);
    row->render = malloc(row->size + tabs * (BABYVIM_TAB_STOP - 1) + 1);

    int idx = 0;
    for (j = 0; j < row->size; j++) {
        if (row->chars[j] == '\t') {
            row->render[idx++] = ' ';
            while (idx % BABYVIM_TAB_STOP != 0) row->render[idx++] = ' ';
        } else {
            row->render[idx++] = row->chars[j];
        }
    }
    row->render[idx] = '\0';
    row->rsize = idx;

    if (BABYVIM_WRAP_LINES == 1 && !FILE_LOADING &&
        !WRAP_IN_PROGRESS && row->rsize >= E.textcols) { // hard wrap
        editorHardWrapRow(row->idx);
        return;
    }

    editorUpdateSyntax(row);
}

void editorInsertRow(int at, char *s, size_t len) {
    if (at < 0 || at > E.numrows) return;

    E.row = realloc(E.row, sizeof(erow) * (E.numrows + 1));
    memmove(&E.row[at + 1], &E.row[at], sizeof(erow) * (E.numrows - at));
    for (int j = at + 1; j <= E.numrows; j++) E.row[j].idx++; // increment all the row indices after the inserted row

    E.row[at].idx = at;
    E.row[at].size = len;
    E.row[at].chars = malloc(len + 1);

    memcpy(E.row[at].chars, s, len);
    E.row[at].chars[len] = '\0';
    
    E.row[at].rsize = 0;
    E.row[at].indent = 0;
    E.row[at].render = NULL;
    E.row[at].hl = NULL;
    E.row[at].hl_open_comment = 0;
    editorUpdateRow(&E.row[at]);

    E.numrows++;
    int linenumwidth = snprintf(NULL, 0, "%d", E.numrows) + 1;
    if (linenumwidth > E.linenumwidth) {
        E.textcols -= linenumwidth - E.linenumwidth;
        E.linenumwidth = linenumwidth;
    }
    E.dirty++;
}

void editorHardWrapRow(int idx) {
    if (idx < 0 || idx >= E.numrows) return;
    erow *row = &E.row[idx];

    if (row->rsize < E.textcols) return; // nothing to wrap

    WRAP_IN_PROGRESS = 1;

    while (row->rsize >= E.textcols) {
        // single line comments and strings must not be wrapped
        if (row->hl[E.textcols] == HL_STRING || row->hl[E.textcols] == HL_COMMENT) break;
        
        // find suitable wrapping point
        int split_cx = editorRowRxToCx(row, E.textcols - 1);
        while (split_cx > 0 && !is_space(row->chars[split_cx])) split_cx--;

        // leading whitespace
        int left_end = split_cx;
        while (left_end > 0 && is_space(row->chars[left_end])) left_end--;

        // trailing whitespace
        int right_start = split_cx;
        while (right_start < row->size && is_space(row->chars[right_start])) right_start++;

        // insert row
        int remainder_len = row->size - right_start;
        editorInsertRow(idx + 1, &row->chars[right_start], remainder_len);

        row = &E.row[idx]; // reassign because of the realloc
        row->size = left_end + 1;
        row->chars[row->size] = '\0';
        editorUpdateRow(row); // new row is updated by insertion
        editorIndentRow(&E.row[idx + 1], row->indent - E.row[idx + 1].indent);

        // if user was typing, reposition the cursor
        if (E.cy == idx && E.cx > left_end) {
            int tail = E.cx - right_start;
            E.cy = idx + 1;
            E.cx = E.row[E.cy].indent + (tail < 0 ? 0 : tail);
        }

        // continue wrapping
        row = &E.row[idx + 1];
    }

    WRAP_IN_PROGRESS = 0;
}

void editorFreeRow(erow *row) {
    free(row->render);
    free(row->chars);
    free(row->hl);
}

void editorDelRow(int at) {
    if (at < 0 || at >= E.numrows) return;
    editorFreeRow(&E.row[at]);
    memmove(&E.row[at], &E.row[at + 1], sizeof(erow) * (E.numrows - at - 1));
    for (int j = at; j < E.numrows - 1; j++) E.row[j].idx--; // decrement all the row indices after the deleted row
    E.numrows--;
    E.dirty++;
}

void editorIndentRow(erow *row, int indent) {
    char *new = malloc(row->size + indent + 1);
    memset(new, ' ', indent);
    memcpy(&new[indent], row->chars, row->size);
    new[row->size + indent] = '\0';
    free(row->chars);
    row->chars = new;
    row->size += indent;
    editorUpdateRow(row);
}

void editorRowInsertChar(erow *row, int at, int c) {
    if (at < 0 || at > row->size) at = row->size;
    row->chars = realloc(row->chars, row->size + 2);
    memmove(&row->chars[at + 1], &row->chars[at], row->size - at + 1);
    row->size++;
    row->chars[at] = c;
    editorUpdateRow(row);
    E.dirty++;
}

void editorRowInsertString(erow *row, int at, char *s, size_t len) {
    if (at < 0 || at > row->size) at = row->size;
    row->chars = realloc(row->chars, row->size + len + 1);
    memcpy(&row->chars[at], s, len);
    row->size += len;
    row->chars[row->size] = '\0';
    editorUpdateRow(row);
    E.dirty++;
}

int editorRowDelChar(erow *row, int at) {
    if (at < 0 || at >= row->size) return 0;
    
    // detect tab at the beginning of the row
    int all_spaces = BABYVIM_SPACE_TYPE && at >= BABYVIM_TAB_STOP - 1;
    int j = at;
    while (all_spaces && j) {
        if (row->chars[j] != ' ') {
            all_spaces = 0;
        }
        j--;
    }
    int mod = (at + 1) % BABYVIM_TAB_STOP;
    int del_count = all_spaces ? (mod == 0 ? BABYVIM_TAB_STOP : mod) : 1;
    
    memmove(&row->chars[at - del_count + 1], &row->chars[at + 1], row->size - at);
    row->size -= del_count;
    editorUpdateRow(row);
    E.dirty++;
    return del_count;
}

/*** editor operations ***/

static inline int getVisualRowCount() {
    int res = 0;
    for (int r = 0; r < E.numrows; r++) res += rowVisualHeight(&E.row[r]);
    return res;
}

static inline void normalizeSelection(int *sx, int *sy, int *ex, int *ey) {
    if (isLessCoord(E.selection.sx, E.selection.sy, E.cx, E.cy)) {
        *sx = E.selection.sx;
        *sy = E.selection.sy;
        *ex = E.cx;
        *ey = E.cy;
    } else {
        *sx = E.cx;
        *sy = E.cy;
        *ex = E.selection.sx;
        *ey = E.selection.sy;
    }
}

static inline void startSelection() {
    E.selection.sx = E.cx;
    E.selection.sy = E.cy;
    E.selection.active = 1;
}

void editorDelSelection() {
    if (!E.selection.active || E.numrows == 0) return;

    int sx, sy, ex, ey;
    normalizeSelection(&sx, &sy, &ex, &ey);

    if (sy == ey) { // delete chars
        erow *row = &E.row[sy];
        memmove(&row->chars[sx], &row->chars[ex], row->size - ex + 1); // terminator
        row->size -= ex - sx;
        editorUpdateRow(row);
    } else {
        erow *rowS = &E.row[sy];
        erow *rowE = &E.row[ey];
        
        int right_len = rowE->size - ex;
        char *right = right_len > 0 ? malloc(right_len + 1) : NULL;
        if (right) memcpy(right, &rowE->chars[ex], right_len);

        // truncate start row
        rowS->size = sx;
        rowS->chars[sx] = '\0';
        editorUpdateRow(rowS);

        // delete rows between
        for (int r = sy + 1; r <= ey; r++) editorDelRow(r);

        if (right_len > 0) {
            editorRowInsertString(rowS, sx, right, right_len);
            free(right);
        }
    }
    
    E.cy = sy; E.cx = sx;
    E.dirty++;
    E.selection.active = 0;
}

void editorCopySelection() {
    if (!E.selection.active || E.numrows == 0) return;
    int sx, sy, ex, ey;
    normalizeSelection(&sx, &sy, &ex, &ey);

    if (sy == ey) { // single line
        erow *row = &E.row[sy];
        int len = ex - sx;
        E.copyBuffer = malloc(len + 1);
        memcpy(E.copyBuffer, &row->chars[sx], len);
        E.copyBuffer[len] = '\0';
    } else { // multi-line
        erow *rowS = &E.row[sy];
        erow *rowE = &E.row[ey];

        // calculate the total length of the selection
        int left_len = rowS->size - sx + 1;
        int right_len = ex + 1;
        int total_len = left_len + right_len;
        for (int r = sy + 1; r < ey; r++) total_len += E.row[r].size;

        int idx = 0;
        E.copyBuffer = malloc(total_len + 1);

        memcpy(E.copyBuffer, &rowS->chars[sx], left_len);
        idx += left_len;
        // copy the middle rows
        for (int r = sy + 1; r < ey; r++) {
            memcpy(E.copyBuffer + idx, E.row[r].chars, E.row[r].size);
            idx += E.row[r].size;
        }
        // copy the right row
        memcpy(E.copyBuffer + idx, &rowE->chars[ex], right_len);
        E.copyBuffer[total_len] = '\0';
    }
}

void editorInsertString(char *s, size_t len) {
    if (E.cy == E.numrows) editorInsertRow(E.numrows, "", 0);
    editorRowInsertString(&E.row[E.cy], E.cx, s, len);
    E.cx += len;
}

void editorPasteSelection() {
    if (E.copyBuffer == NULL) return;
    editorInsertString(E.copyBuffer, strlen(E.copyBuffer));
}

void editorInsertChar(int c) {
    if (E.selection.active) editorDelSelection();

    if (E.cy == E.numrows) editorInsertRow(E.numrows, "", 0);
    editorRowInsertChar(&E.row[E.cy], E.cx, c);
    E.cx++;
}

void editorInsertNewline() {
    if (E.cx == 0) {
        editorInsertRow(E.cy, "", 0);
        E.cx = 0;
    } else {
        erow *row = &E.row[E.cy];
        editorInsertRow(E.cy + 1, &row->chars[E.cx], row->size - E.cx);
        row = &E.row[E.cy]; // reassign because of the realloc
        row->size = E.cx;
        row->chars[row->size] = '\0';
        editorUpdateRow(row); // editorInsertRow calls editorUpdateRow for the new row

        editorIndentRow(&E.row[E.cy + 1], row->indent);
        E.cx = row->indent;
    }
    E.cy++;
}

void editorDelChar() {
    if (E.cy == E.numrows) return;
    erow *row = &E.row[E.cy];
    if (E.cx > 0) {
        E.cx -= editorRowDelChar(row, E.cx - 1);
    } else {
        if (E.cy == 0) return;
        E.cx = E.row[E.cy - 1].size;
        editorRowInsertString(&E.row[E.cy - 1], E.cx, row->chars, row->size);
        editorDelRow(E.cy);
        E.cy--;
    }
}

/*** file i/o ***/

char *editorRowsToString(int *buflen) {
    int totlen = 0;
    int j = 0;
    for (j = 0; j < E.numrows; j++)
        totlen += E.row[j].size + 1;
    *buflen = totlen;

    char *buf = malloc(totlen);
    char *p = buf;
    for (j = 0; j < E.numrows; j++) {
        memcpy(p, E.row[j].chars, E.row[j].size);
        p += E.row[j].size;
        *p = '\n';
        p++;
    }
    return buf;
}

void editorOpen(char *filename) {
    free(E.filename);
    E.filename = strdup(filename);

    editorSelectSyntaxHighlight();

    FILE *fp = fopen(filename, "r");
    if (!fp) {
        E.dirty = 1; // set dirty flag for non-existent file
        return;
    }; // if file doesn't exist, just set syntax highlighting and dirty flag

    FILE_LOADING = 1;

    char *line = NULL;
    size_t linecap = 0;
    ssize_t linelen;
    while ((linelen = getline(&line, &linecap, fp)) != -1) {
        while (linelen > 0 && (line[linelen - 1] == '\n' ||
                               line[linelen - 1] == '\r'))
            linelen--;
        editorInsertRow(E.numrows, line, linelen);
    }

    free(line);
    fclose(fp);
    E.dirty = 0;
    FILE_LOADING = 0;

    // wrap pass
    if (BABYVIM_WRAP_LINES == 1) {
        for (int j = 0; j < E.numrows; j++) {
            editorHardWrapRow(j);
        }
    }
}

void editorClose() {
    write(STDOUT_FILENO, "\x1b[2J", 4); // Clear screen
    write(STDOUT_FILENO, "\x1b[H", 3); // Move cursor to top-left corner
    exit(0);
}

void editorSave() {
    if (E.filename == NULL) {
        E.filename = editorPrompt("Save as: %s (ESC to cancel)", NULL);
        if (E.filename == NULL) {
            editorSetStatusMessage(1, "Save aborted");
            return;
        }
        editorSelectSyntaxHighlight();
    }

    int len;
    char *buf = editorRowsToString(&len);

    int fd = open(E.filename, O_RDWR | O_CREAT, 0644);
    if (fd != -1) {
        if (ftruncate(fd, len) != -1) {
            if (write(fd, buf, len) == len) {
                close(fd);
                free(buf);
                E.dirty = 0;
                editorSetStatusMessage(1, "%d bytes written to disk", len);
                return;
            }
        }
        close(fd);
    }
    free(buf);
    editorSetStatusMessage(1, "Can't save! I/O error: %s", strerror(errno));
}

/*** find ***/

void editorFindCallback(char *query, int key) {
    static int last_match = -1;
    static int direction = 1;

    static int saved_hl_line;
    static char *saved_hl = NULL;

    // restore the previous highlight
    if (saved_hl) {
        memcpy(E.row[saved_hl_line].hl, saved_hl, E.row[saved_hl_line].rsize);
        free(saved_hl);
        saved_hl = NULL;
    }

    if (key == '\r' || key == '\x1b') {
        last_match = -1;
        direction = 1;
        return;
    } else if (key == ARROW_RIGHT || key == ARROW_DOWN) {
        direction = 1;
    } else if (key == ARROW_LEFT || key == ARROW_UP) {
        direction = -1;
    } else {
        last_match = -1;
        direction = 1;
    }

    if (last_match == -1) direction = 1;
    int current = last_match;


    for (int i = 0; i < E.numrows; i++) {
        current += direction;
        if (current == -1) current = E.numrows - 1;
        else if (current == E.numrows) current = 0;
        
        erow *row = &E.row[current];
        char *match = strstr(row->render, query);
        if (match) {
            // record the match and move the cursor to it
            last_match = current;
            E.cy = current;
            E.cx = editorRowRxToCx(row, match - row->render);
            E.rowoff = E.numrows;

            // save the current highlight data
            saved_hl_line = current;
            saved_hl = malloc(row->rsize);
            memcpy(saved_hl, row->hl, row->rsize);

            // highlight the match
            memset(&row->hl[match - row->render], HL_MATCH, strlen(query));
            break;
        }
    }
}

void editorFind() {
    int saved_cx = E.cx;
    int saved_cy = E.cy;
    int saved_coloff = E.coloff;
    int saved_rowoff = E.rowoff;

    char *query = editorPrompt("Search: %s (Use ESC/Arrows/Enter)",
                             editorFindCallback);
    
    if (query) {
        free(query);
    } else {
        E.cx = saved_cx;
        E.cy = saved_cy;
        E.coloff = saved_coloff;
        E.rowoff = saved_rowoff;
    }
}

/*** normal mode ***/

static char *skip_ws(char *s) { while (*s && isspace(*s)) s++; return s;}

static void rtrim_inplace(char *s) {
    if (!s) return;
    char *end = s + strlen(s) - 1;
    while (end >= s && isspace(*end)) end--;
    end[1] = '\0';
}

static char *trim_inplace(char *s) { rtrim_inplace(s); return skip_ws(s); }

static int starts_with(const char *s, const char *prefix) {
    while (*prefix && *s) {
        char a = tolower(*s++);
        char b = tolower(*prefix++);
        if (a != b) return 0;
    }
    return *prefix == '\0';
}

static int parse_long(const char *s, long *out) {
    char *end = NULL;
    long v = strtol(s, &end, 10);
    if (end == s) return 0;
    *out = v;
    return 1;
}

void editorNormalModeCallback(char *query, int key) {
    if (key != '\r') return;

    char *cmd = trim_inplace(query);
    if (*cmd == '\0') { E.commandEntering = 0; return; }

    // single chars
    if ((cmd[0] == 'w' || cmd[0] == 'q' || cmd[0] == 'l') && cmd[1] == '\0') {
        if (cmd[0] == 'w') editorSave();
        else if (cmd[0] == 'q') editorClose();
        else if (cmd[0] == 'l') editorSetStatusMessage(1, "\x1b[30;101mUsage: l <line number>\x1b[m");
        E.commandEntering = 0;
        return;
    }

    // special line jumps
    if (cmd[0] == '$' && cmd[1] == '\0') { 
        E.cy = getVisualRowCount();
        E.rowoff = getVisualRowCount() - E.textrows;
        E.commandEntering = 0;
        return;
    }

    // unrecognized command
    editorSetStatusMessage(1, "\x1b[101mUnrecognized command: %s\x1b[m", cmd);
    E.commandEntering = 0;
}

/*** output ***/

void editorScroll() {
    E.rx = 0;
    if (E.cy < E.numrows) E.rx = editorRowCxToRx(&E.row[E.cy], E.cx);

    if (BABYVIM_WRAP_LINES == 2 && E.cy < E.numrows) {
        erow *row = &E.row[E.cy];
        int sub = rowSubrowAtRx(row, E.rx);
        int cursor_vrow = rowAbsoluteVisualRow(E.cy, sub);
        if (cursor_vrow < E.rowoff) E.rowoff = cursor_vrow;
        if (cursor_vrow >= E.rowoff + E.textrows) E.rowoff = cursor_vrow - E.textrows + 1;

        E.coloff = 0;
        return;
    }

    if (E.cy < E.rowoff) E.rowoff = E.cy;
    if (E.cy >= E.rowoff + E.textrows) E.rowoff = E.cy - E.textrows + 1;
    if (E.rx < E.coloff) E.coloff = E.rx;
    if (E.rx >= E.coloff + E.textcols) E.coloff = E.rx - E.textcols + 1;
}

static inline int isCellSelected(int filerow, int cx) {
    if (!E.selection.active) return 0;
    if (E.selection.sx == E.cx && E.selection.sy == E.cy) return 0;

    int sx, sy, ex, ey;
    normalizeSelection(&sx, &sy, &ex, &ey);

    return (isLessCoord(sx, sy, cx, filerow) &&
           isLessCoord(cx, filerow, ex, ey)) ||
           (sx == cx && sy == filerow);
}

void editorDrawRow(struct abuf *ab, erow *row, int coloff, int rowlen) {
    char *c = &row->render[coloff];
    unsigned char *hl = &row->hl[coloff];
    int current_color = -1;
    int selected = 0;
    for (int j = 0; j < rowlen; j++) {
        if (iscntrl(c[j])) { // control characters inverted color
            char sym = (c[j] <= 26) ? '@' + c[j] : '?';
            abAppend(ab, "\x1b[7m", 4);
            abAppend(ab, &sym, 1);
            abAppend(ab, "\x1b[m", 3); // reset default formatting
            if (current_color != -1) { // restore color
                char buf[16];
                int clen = snprintf(buf, sizeof(buf), "\x1b[%dm", current_color);
                abAppend(ab, buf, clen);
            }
        } else {
            // selection
            int x = editorRowRxToCx(row, coloff + j);
            int y = row->idx;
            if (!selected && isCellSelected(y, x)) {
                selected = 1;
                abAppend(ab, "\x1b[7m", 4);
            } else if (!isCellSelected(y, x)) {
                selected = 0;
                abAppend(ab, "\x1b[27m", 5);
            }
            
            if (hl[j] == HL_NORMAL) {
                if (current_color != -1) { // reset color
                    abAppend(ab, "\x1b[39m", 5);
                    current_color = -1;
                }
                abAppend(ab, &c[j], 1);
            } else {
                int color = editorSyntaxToColor(hl[j]);
                if (current_color != color) { // only change color if it's different
                    current_color = color;
                    char buf[16];
                    int clen = snprintf(buf, sizeof(buf), "\x1b[%dm", color);
                    abAppend(ab, buf, clen);
                }
                abAppend(ab, &c[j], 1);
            }
        }
    }
    abAppend(ab, "\x1b[m", 3); // restore default formatting
    if (rowlen < E.textcols) abAppend(ab, "\x1b[K", 3); // clear the line
    abAppend(ab, "\r\n", 2); // print the new line
}

void editorDrawSidebar(struct abuf *ab, int *rownum) {
    char buf[16];
    int len = E.linenumwidth;
    if (rownum == NULL) {
        memset(buf, ' ', E.linenumwidth);
        buf[E.linenumwidth] = '\0';
    } else {
        len = snprintf(buf, sizeof(buf),
            "\x1b[90m%-*d\x1b[m", E.linenumwidth, *rownum + 1);
    }
    abAppend(ab, buf, len);
}

void editorDrawRows(struct abuf *ab) {
    int cur_v = E.rowoff;
    int y_limit = E.numrows;
    if (BABYVIM_WRAP_LINES == 2) {
        int total = 0; 
        for (int i = 0; i < E.numrows; i++) total += rowVisualHeight(&E.row[i]);
        y_limit = total;
    }

    for (int y = 0; y < E.textrows; y++) {
        // lines starting with ~
        if (cur_v >= y_limit) {
            if (E.numrows == 0 &&y == E.textrows / 3) { // print welcome message
                char welcome[80];
                int welcomelen = snprintf(welcome, sizeof(welcome),
                    "Babyvim -- version %s", BABYVIM_VERSION);

                // Handle not enough width
                if (welcomelen > E.textcols) welcomelen = E.textcols;

                // Center the welcome message
                int padding = (E.textcols - welcomelen) / 2;
                if (padding) {
                    abAppend(ab, "\x1b[90m~", 6);
                    abAppend(ab, "\x1b[m", 3);
                    padding--;
                }
                while (padding--) abAppend(ab, " ", 1);
                abAppend(ab, welcome, welcomelen);
                abAppend(ab, "\x1b[K\r\n", 5);
            } else {
                abAppend(ab, "\x1b[90m~\x1b[m\x1b[K\r\n", 14);
            }
            continue;
        }

        int filerow, subrow;
        visualIndexToFile(cur_v, &filerow, &subrow);
        erow *row = (filerow >= 0 && filerow < E.numrows) ? &E.row[filerow] : NULL;

        // Print line number
        editorDrawSidebar(ab, subrow == 0 ? &filerow : NULL);

        int coloff = E.coloff + subrow * E.textcols;
        int remain = row->size - coloff;
        int take = remain > 0 ? (remain < E.textcols ? remain : E.textcols) : 0;
        editorDrawRow(ab, row, coloff, take);
        cur_v++;
    }
}

void editorDrawStatusBar(struct abuf *ab) {
    abAppend(ab, "\x1b[7m", 4);

    char status[80], rstatus[80];
    int len = snprintf(status, sizeof(status), "%.20s - %d lines %s",
        E.filename ? E.filename : "[No Name]", E.numrows,
        E.dirty ? "(modified)" : "");
    int rlen = snprintf(rstatus, sizeof(rstatus), "%s | %d:%d/%d",
        E.syntax ? E.syntax->filetype : "no ft", E.cy + 1, E.cx, E.numrows);

    if (len > E.screencols) len = E.screencols;
    abAppend(ab, status, len);
    while (len < E.screencols) {
        if (E.screencols - len == rlen) {
            abAppend(ab, rstatus, rlen);
            break;
        } else {
            abAppend(ab, " ", 1);
            len++;
        }
    }
    abAppend(ab, "\x1b[m", 3);
    abAppend(ab, "\r\n", 2);
}

void editorDrawMessageBar(struct abuf *ab) {
    abAppend(ab, "\x1b[K", 3);
    int msglen = strlen(E.statusmsg);
    if (msglen > E.screencols) msglen = E.screencols;
    if (msglen && (E.statusmsg_time == -1 || time(NULL) - E.statusmsg_time < 5))
        abAppend(ab, E.statusmsg, msglen);
}

void editorRefreshScreen() {
    editorScroll();

    struct abuf ab = ABUF_INIT;

    abAppend(&ab, "\x1b[?25l", 6); // Hide cursor
    abAppend(&ab, "\x1b[H", 3); // Move cursor to top-left corner
    
    editorDrawRows(&ab);
    editorDrawStatusBar(&ab);
    editorDrawMessageBar(&ab);

    // Move cursor to the current position
    refreshCursorPosition(&ab);

    abAppend(&ab, "\x1b[?25h", 6); // Show cursor

    write(STDOUT_FILENO, ab.b, ab.len);
    abFree(&ab);
}

void editorSetStatusMessage(int clear, const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    vsnprintf(E.statusmsg, sizeof(E.statusmsg), fmt, ap);
    va_end(ap);
    E.statusmsg_time = clear ? time(NULL) : -1;
}

/*** input ***/

char *editorPrompt(char *prompt, void (*callback)(char *, int)) {
    size_t bufsize = 128;
    char *buf = malloc(bufsize);

    size_t buflen = 0;
    buf[0] = '\0';

    while (1) {
        editorSetStatusMessage(0, prompt, buf);
        editorRefreshScreen();

        int c = editorReadKey();
        if (c == DEL_KEY || c == CTRL_KEY('h') || c == BACKSPACE) {
            if (buflen != 0) buf[--buflen] = '\0';
        } else if (c == '\x1b') {
            editorSetStatusMessage(1, "");
            if (callback) callback(buf, c);
            free(buf);
            return NULL;
        } else if (c == '\r') {
            if (buflen != 0) {
                editorSetStatusMessage(1, "");
                if (callback) callback(buf, c);
                return buf;
            }
        } else if (!iscntrl(c) && c < 128) {
            if (buflen == bufsize - 1) {
                bufsize *= 2;
                buf = realloc(buf, bufsize);
            }
            buf[buflen++] = c;
            buf[buflen] = '\0';
        }

        if (callback) callback(buf, c);
    }
}

void editorMoveCursor(int key) {
    key = translateShiftKey(key);

    static int desired_col_in_subrow = -1;
    erow *row = (E.cy < E.numrows) ? &E.row[E.cy] : NULL;
    
    // ensure rx is up to date
    if (row) E.rx = editorRowCxToRx(row, E.cx);

    if (BABYVIM_WRAP_LINES == 2 && row) {
        int subrow = rowSubrowAtRx(row, E.rx);
        int col_in_subrow = rowcolInSubrow(row, E.rx);
        if (desired_col_in_subrow < 0) desired_col_in_subrow = col_in_subrow;

        if (key == ARROW_UP || key == ARROW_DOWN) {
            int tgt_row = E.cy;
            int tgt_sub = subrow + (key == ARROW_UP ? -1 : 1);

            if (tgt_sub < 0) {
                // go to previous row's last subrow
                if (E.cy > 0) {
                    tgt_row--;
                    erow *prev_row = &E.row[tgt_row];
                    tgt_sub = rowVisualHeight(prev_row) - 1;
                } else {
                    tgt_sub = 0;
                }
            } else if (tgt_sub >= rowVisualHeight(row)) {
                // go to next row's first subrow
                if (E.cy < E.numrows - 1) {
                    tgt_row++;
                    tgt_sub = 0;
                } else {
                    tgt_sub = rowVisualHeight(row) - 1;
                }
            }

            // move
            E.cy = tgt_row;
            erow *trow = (E.cy < E.numrows) ? &E.row[E.cy] : NULL;
            int base_rx = tgt_sub * E.textcols;
            int target_rx = base_rx + desired_col_in_subrow;
            if (trow) {
                if (target_rx > trow->size) target_rx = trow->size;
                E.cx = editorRowRxToCx(trow, target_rx);
            }
            return;
        }

        // left and right must behave normally
        switch (key) {
            case ARROW_LEFT:
                if (E.cx != 0) {
                    E.cx--;
                } else if (E.cy > 0) { // Move to the previous line
                    E.cy--;
                    E.cx = E.row[E.cy].size;
                }
                break;
            case ARROW_RIGHT:
                if (row && E.cx < row->size) { // Move to the next character
                    E.cx++;
                } else if (E.cy < E.numrows) { // Move to the next line
                    E.cy++;
                    E.cx = 0;
                }
                break;
        }

        // update desired visual column
        row = (E.cy < E.numrows) ? &E.row[E.cy] : NULL;
        if (row) {
            E.rx = editorRowCxToRx(row, E.cx);
            desired_col_in_subrow = rowcolInSubrow(row, E.rx);
        }
        return;
    }

    // Original behavior
    switch (key) {
        case ARROW_LEFT:
            if (E.cx != 0) {
                E.cx--;
            } else if (E.cy > 0) { // Move to the previous line
                E.cy--;
                E.cx = E.row[E.cy].size;
            }
            break;
        case ARROW_RIGHT:
            if (row && E.cx < row->size) { // Move to the next character
                E.cx++;
            } else if (E.cy < E.numrows) { // Move to the next line
                E.cy++;
                E.cx = 0;
            }
            break;
        case ARROW_UP:
            if (E.cy == 0) break;
            E.cy--;
            break;
        case ARROW_DOWN:
            if (E.cy >= E.numrows) break;
            E.cy++;
            break;
    }

    // Snapping to the end of the line
    row = (E.cy >= E.numrows) ? NULL : &E.row[E.cy];
    int rowlen = row ? row->size : 0;
    if (E.cx > rowlen) E.cx = rowlen;
}

void editorProcessKeypress() {
    static int quit_times = BABYVIM_QUIT_TIMES;

    int c = editorReadKey();

    // globals
    switch (c) {
        case CTRL_KEY('q'):
            if (E.dirty && quit_times > 0) {
                editorSetStatusMessage(1, "WARNING!!! File has unsaved changes. Press Ctrl-Q %d more times to quit.", quit_times);
                quit_times--;
                return;
            }
            editorClose();
            break;
    
        case CTRL_KEY('s'):
            editorSave();
            break;

        case CTRL_KEY('f'):
            editorFind();
            break;
    }

    // normal mode
    if (E.mode == NORMAL_MODE) {
        switch (c) {
            case 'i':
                E.mode = INSERT_MODE;
                editorSetStatusMessage(0, "\x1b[7m--INSERT--\x1b[m");
                break;
            case ':': 
                {
                    E.commandEntering = 1;
                    char *query = editorPrompt(":%s", editorNormalModeCallback);
                    if (query) free(query);
                }
                break;
        }
        return;
    }

    // insert mode
    switch (c) {
        case '\r':
            editorInsertNewline();
            break;
        case '\t':
            if (BABYVIM_SPACE_TYPE) {
                int j = BABYVIM_TAB_STOP;
                while (j--) editorInsertChar(' ');
                break;
            } else {
                editorInsertChar(c);
                break;
            }

        case CTRL_KEY('c'):
            if (E.selection.active) editorCopySelection();            
            break;

        case CTRL_KEY('x'):
            if (E.selection.active) editorCopySelection();
            editorDelSelection();
            break;

        case CTRL_KEY('v'):
            editorPasteSelection();
            break;

        case SHIFT_KEY(ARROW_UP):
        case SHIFT_KEY(ARROW_DOWN):
        case SHIFT_KEY(ARROW_RIGHT):
        case SHIFT_KEY(ARROW_LEFT):
            if (!E.selection.active) startSelection();
        case ARROW_UP:
        case ARROW_DOWN:
        case ARROW_LEFT:
        case ARROW_RIGHT:
            editorMoveCursor(c);
            break;  

        case PAGE_UP:
        case PAGE_DOWN:
        {
            if (c == PAGE_UP) {
                E.cy = E.rowoff;
            } else {
                E.cy = E.rowoff + E.screenrows - 1;
                if (E.cy > E.numrows) E.cy = E.numrows - 1;
            }

            int times = E.screenrows;
            while (times--) editorMoveCursor(c == PAGE_UP ? ARROW_UP : ARROW_DOWN);
            break;
        }

        case SHIFT_KEY(HOME_KEY):
            if (!E.selection.active) startSelection();
        case HOME_KEY:
            E.cx = E.row[E.cy].indent;
            break;

        case SHIFT_KEY(END_KEY):
            if (!E.selection.active) startSelection();
        case END_KEY:
            if (E.cy < E.numrows)
                E.cx = E.row[E.cy].size;
            break;

        case BACKSPACE:
        case CTRL_KEY('h'):
        case DEL_KEY:
            if (E.selection.active) {
                editorDelSelection();
                E.selection.active = 0;
                break;
            }
            if (c == DEL_KEY) editorMoveCursor(ARROW_RIGHT);
            editorDelChar();
            break;

        case CTRL_KEY('l'):
        case '\x1b':
            E.mode = NORMAL_MODE;
            break;

        default:
            editorInsertChar(c);
            break;
    }

    if (!isShiftKey(c)) E.selection.active = 0;
    quit_times = BABYVIM_QUIT_TIMES;
}

/*** init ***/

void initEditor() {
    E.mode = NORMAL_MODE;
    E.commandEntering = 0;
    E.cx = 0;
    E.cy = 0;
    E.rx = 0;
    E.rowoff = 0;
    E.coloff = 0;
    E.numrows = 0;
    E.linenumwidth = 2; // ~ character + space
    E.row = NULL;
    E.dirty = 0;
    E.filename = NULL;
    E.statusmsg[0] = '\0';
    E.statusmsg_time = 0;
    E.syntax = NULL;

    struct editorSelection selection = EDITOR_SELECTION_INIT;
    E.selection = selection;
    E.copyBuffer = NULL;

    if (getWindowSize(&E.screenrows, &E.screencols) == -1) die("getWindowSize");
    E.textrows = E.screenrows - 2; // for the status line
    E.textcols = E.screencols - E.linenumwidth; // for the line number area
}

int main(int argc, char *argv[]) {
    enableRawMode();
    initEditor();
    if (argc >= 2) {
        editorOpen(argv[1]);
        if (argc >= 3) {
            if (argv[2][0] == '+' || argv[2][0] == '-') {
                E.mode = argv[2][0] == '+' ? INSERT_MODE : NORMAL_MODE;
                int y = atoi(&argv[2][1]) - 1;
                y = y < 0 ? 0 : y;
                E.cy = y;
                E.rowoff = y;
            }
        }
    }

    editorSetStatusMessage(1, "HELP: Ctrl-S = save | Ctrl-Q = quit | Ctrl-F = find");

    while (1) {
        editorRefreshScreen();
        editorProcessKeypress();
    }
    return 0;
}
