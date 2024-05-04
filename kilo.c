/*** includes ***/

#define _DEFAULT_SOURCE
#define _BSD_SOURCE
#define _GNU_SOURCE

#include <ctype.h>
#include <errno.h>
#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <termios.h>
#include <time.h>
#include <unistd.h>

/*** debug logging ***/

#define DEBUG_FILE_LOG 1

int log_file = -1;

void closeLogFile() {
  if (log_file != -1)
    close(log_file);
}

void initLogger() {
  atexit(closeLogFile);
  log_file = open("editor.log", O_RDWR | O_CREAT, 0644);
  assert(log_file != -1);
  ftruncate(log_file, 0);
}

char log_buffer[200];

void debug_log(const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vsnprintf(log_buffer, sizeof(log_buffer), fmt, ap);
  va_end(ap);
  assert(log_file != -1);
  write(log_file, log_buffer, strlen(log_buffer));
  write(log_file, "\n", 1);
}

#if DEBUG_FILE_LOG
  #define DEBUG_LOG(...) debug_log(__VA_ARGS__)
#else
  #define DEBUG_LOG(...)
#endif

/*** defines ***/

#define KILO_VERSION "0.0.1"
#define KILO_TAB_STOP 8
#define KILO_QUIT_TIMES 1

#define CTRL_KEY(k) ((k) & 0x1f)

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

enum editorHighlight {
  HL_NORMAL = 0,
  HL_COMMENT,
  HL_MLCOMMENT,
  HL_KEYWORD1,
  HL_KEYWORD2,
  HL_STRING,
  HL_NUMBER,
  HL_MATCH
};

#define HL_HIGHLIGHT_NUMBERS  (1<<0)
#define HL_HIGHLIGHT_STRINGS  (1<<1)

/*** append buffer ***/

struct abuf {
  char *b;
  int len;
};

#define ABUF_INIT {NULL, 0}

#define abAppendLit(ab, s) abAppend(ab, s, sizeof(s) - 1)

void abAppend(struct abuf *ab, char *s, int len) {
  char *new = realloc(ab->b, ab->len + len);
  assert(new != NULL);
  memcpy(&new[ab->len], s, len);
  ab->b = new;
  ab->len += len;
}

void abFree(struct abuf *ab) {
  free(ab->b);
}

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

struct editorRow {
  int size;
  int render_size;
  char *chars;
  char *render;
  char *hl;
  int hl_open_comment;
};

struct editorConfig {
  int cx;
  int cy;
  int rx;
  int row_offset;
  int col_offset;
  int screen_rows;
  int screen_cols;
  int num_rows;
  struct editorRow *rows;
  int dirty;
  char *file_name;
  char statusmsg[80];
  time_t statusmsg_time;
  struct editorSyntax *syntax;
  struct termios orig_termios;
};

struct editorConfig E;

/*** filetypes ***/

char *C_HL_extensions[] = { ".c", ".h", ".cpp", NULL };
char *C_HL_keywords[] = {
  "switch", "if", "while", "for", "break", "continue", "return", "else",
  "struct", "union", "typedef", "static", "enum", "class", "case",

  "int|", "long|", "double|", "float|", "char|", "unsigned|", "signed|", "void|",
  NULL
};

struct editorSyntax HLDB[] = {
  {
    "c",
    C_HL_extensions,
    C_HL_keywords,
    "//",
    "/*",
    "*/",
    HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS
  },
};

#define HLDB_ENTRIES 1

/*** prototypes ***/

void editorSetStatusMessage(const char *fmt, ...);
void editorRefreshScreen();
char *editorPrompt(char *prompt, void (*callback)(char *, int));

/*** terminal ***/

void die(const char *s) {
  write(STDOUT_FILENO, "\x1b[2J\x1b[H", 7); // Erase In Display, Cursor Position
  perror(s);
  exit(1);
}

void restoreOrigTermios() {
  DEBUG_LOG("restoring original terminal attributes");

  if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &E.orig_termios) == -1)
    die("[ERROR] disableRawMode tcsetattr");
}

void readOrigTermios() {
  DEBUG_LOG("reading original terminal attributes");

  if (tcgetattr(STDIN_FILENO, &E.orig_termios) == -1)
    die("[ERROR] enableRawMode tcgetattr");
}

void enableRawMode() {
  readOrigTermios();
  atexit(restoreOrigTermios);

  struct termios raw = E.orig_termios;
  raw.c_iflag &= ~(BRKINT | ICRNL |INPCK | ISTRIP | IXON);
  raw.c_oflag &= ~(OPOST);
  raw.c_cflag |= (CS8);
  raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
  raw.c_cc[VMIN]=0;
  raw.c_cc[VTIME] = 1;

  if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw) == -1)
    die("[ERROR] enableRawMode tcsetattr");
}

int editorReadKey() {
  int n_read;
  char c;
  while ((n_read = read(STDIN_FILENO, &c, 1)) != 1) {
    if (n_read == -1  && errno != EAGAIN)
      die("[ERROR] editorReadKey read");
  }

  if (c == '\x1b') {
    char seq[3];

    if (read(STDIN_FILENO, &seq[0], 1) != 1)
      return c;
    if (read(STDIN_FILENO, &seq[1], 1) != 1)
      return c;

    if (seq[0] == '[') {
      if (seq[1] > '0' && seq[1] <= '9') {
        if (read(STDIN_FILENO, &seq[2], 1) != 1)
          return c;
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
        }
      } else {
        switch (seq[1]) {
          case 'A': return ARROW_UP;
          case 'B': return ARROW_DOWN;
          case 'C': return ARROW_RIGHT;
          case 'D': return ARROW_LEFT;
          case 'H': return HOME_KEY;
          case 'F': return END_KEY;
        }
      }
    } else if (seq[0] == 'O') {
      switch (seq[1]) {
        case 'H': return HOME_KEY;
        case 'F': return END_KEY;
      }
    }
  }

  return c;
}

int getCursorPosition(int *rows, int *cols) {
  int terminal_response_buffer_size = 32;
  char terminal_response_buffer[32];
  int i = 0;

  if (write(STDOUT_FILENO, "\x1b[6n", 4) != 4) // Device Status Report - cursor position
    return -1;

  while (i < terminal_response_buffer_size - 1) {
    if (read(STDIN_FILENO, &terminal_response_buffer[i], 1) != 1)
      break;
    if (terminal_response_buffer[i] == 'R')
      break;
    i++;
  }
  terminal_response_buffer[i] = '\0';

  if (terminal_response_buffer[0] != '\x1b' || terminal_response_buffer[1] != '[')
    return -1;

  if (sscanf(&terminal_response_buffer[2], "%d;%d", rows, cols) != 2)
    return -1;

  return 0;
}

int getWindowSize(int *rows, int *cols) {
  struct winsize ws;

  if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == -1 || ws.ws_col == 0) {
    if (write(STDOUT_FILENO, "\x1b[999C\x1b[999B", 12) != 12) // Cursor Forward, Cursor Down
      return -1;
    return getCursorPosition(rows, cols);
  }

  *cols = ws.ws_col;
  *rows = ws.ws_row;
  return 0;
}

/*** syntax highlighting ***/

int is_separator(int c) {
  return isspace(c) || c == '\0' || strchr(",.()+-/*=~%<>[];", c) != NULL;
}

void editorUpdateSyntax(struct editorRow *row, int row_idx) {
  if (row->render_size != 0){
    row->hl = realloc(row->hl, row->render_size);
    assert(row->hl != NULL);
    memset(row->hl, HL_NORMAL, row->render_size);
  } else if (row->hl != NULL) {
    free(row->hl);
    row->hl = NULL;
  }

  if (E.syntax == NULL) return;

  char **keywords = E.syntax->keywords;

  char *scs = E.syntax->singleline_comment_start;
  int scs_len = scs ? strlen(scs) : 0;
  char *mcs = E.syntax->multiline_comment_start;
  int mcs_len = mcs ? strlen(mcs) : 0;
  char *mce = E.syntax->multiline_comment_end;
  int mce_len = mce ? strlen(mce) : 0;

  int prev_sep = 1;
  int in_string = 0;
  int in_multiline_comment = (row_idx > 0 && E.rows[row_idx - 1].hl_open_comment);

  int i = 0;
  while (i < row->render_size) {
    char c = row->render[i];
    char prev_hl = (i > 0) ? row->hl[i - 1] : HL_NORMAL;

    if (scs_len && !in_string && !in_multiline_comment) {
      if (!strncmp(&row->render[i], scs, scs_len)) {
        memset(&row->hl[i], HL_COMMENT, row->render_size - i);
        break;
      }
    }

    if (mcs_len && mce_len && !in_string) {
      if (in_multiline_comment) {
        row->hl[i] = HL_MLCOMMENT;
        if (!strncmp(&row->render[i], mce, mce_len)) {
          memset(&row->hl[i], HL_MLCOMMENT, mce_len);
          i += mce_len;
          in_multiline_comment = 0;
          prev_sep = 1;
          continue;
        } else {
          i++;
          continue;
        }
      } else if(!strncmp(&row->render[i], mcs, mcs_len)) {
        memset(&row->hl[i], HL_MLCOMMENT, mcs_len);
        i += mcs_len;
        in_multiline_comment = 1;
        continue;
      }
    }

    if (E.syntax->flags & HL_HIGHLIGHT_STRINGS) {
      if (in_string) {
        row->hl[i] = HL_STRING;
        if (c == '\\' && i + 1 < row->render_size) {
          row->hl[i + 1] = HL_STRING;
          i += 2;
          continue;
        }
        if (c == in_string)
          in_string = 0;
        i++;
        prev_sep = 1;
        continue;
      } else {
        if (c == '"' || c == '\'') {
          in_string = c;
          row->hl[i] = HL_STRING;
          i++;
          continue;
        }
      }
    }

    if (E.syntax->flags & HL_HIGHLIGHT_NUMBERS) {
      if ((isdigit(c) && (prev_sep || prev_hl == HL_NUMBER)) ||
          (c == '.' && prev_hl == HL_NUMBER)) {
        row->hl[i] = HL_NUMBER;
        i++;
        prev_sep = 0;
        continue;
      }
    }

    if (prev_sep) {
      int j;

      for (j = 0; keywords[j]; j++) {
        int keyword_length = strlen(keywords[j]);
        int kw2 = keywords[j][keyword_length - 1] == '|';
        if (kw2) keyword_length--;

        if (!strncmp(&row->render[i], keywords[j], keyword_length) &&
            is_separator(row->render[i + keyword_length])) {
          memset(&row->hl[i], kw2 ? HL_KEYWORD2 : HL_KEYWORD1, keyword_length);
          i += keyword_length;
          break;
        }
      }

      if (keywords[j] != NULL) {
        prev_sep = 0;
        continue;
      }
    }

    prev_sep = is_separator(c);
    i++;
  }

  int changed = (row->hl_open_comment != in_multiline_comment);
  row->hl_open_comment = in_multiline_comment;
  if (changed && row_idx + 1 < E.num_rows)
    editorUpdateSyntax(&E.rows[row_idx + 1], row_idx + 1);
}

int editorSyntaxToColor(int hl) {
  switch (hl) {
    case HL_NUMBER: return 31; // red
    case HL_KEYWORD2: return 32; // green
    case HL_KEYWORD1: return 33; // yellow
    case HL_MATCH: return 34; // blue
    case HL_STRING: return 35; // magenta
    case HL_COMMENT: return 36; // cyan
    case HL_MLCOMMENT: return 36; // cyan
    default: return 37; // white
  }
}

void editorSelectSyntaxHighlight() {
  E.syntax = NULL;
  if (E.file_name == NULL) return;

  char *ext = strrchr(E.file_name, '.');

  for (int j = 0; j < HLDB_ENTRIES; j++) {
    struct editorSyntax *s = &HLDB[j];
    int i = 0;
    while (s->filematch[i]) {
      int is_ext = (s->filematch[i][0] == '.');
      if ((is_ext && ext && !strcmp(ext, s->filematch[i])) ||
          (!is_ext && strstr(E.file_name, s->filematch[i]))) {
        E.syntax = s;

        int row_idx;
        for (row_idx = 0; row_idx < E.num_rows; row_idx++) {
          editorUpdateSyntax(&E.rows[row_idx], row_idx);
        }

        return;
      }
      i++;
    }
  }
}

/*** row operations ***/

int editorRowCxToRx(struct editorRow *row, int cx) {
  int rx = 0;
  int j;
  for(j = 0; j < cx; j++) {
    if (row->chars[j] == '\t')
      rx += (KILO_TAB_STOP - 1) - (rx % KILO_TAB_STOP);
    rx++;
  }
  return rx;
}

int editorRowRxToCx(struct editorRow *row, int rx) {
  int cur_rx = 0;
  int cx;
  for (cx = 0; cx < row->size; cx++) {
    if (row->chars[cx] == '\t')
      cur_rx += (KILO_TAB_STOP - 1) - (cur_rx % KILO_TAB_STOP);
    cur_rx++;

    if (cur_rx > rx) return cx;
  }
  return cx;
}

void editorUpdateRow(struct editorRow *row, int row_idx) {
  int tabs = 0;
  int j;
  for (j = 0; j < row->size; j++)
    if (row->chars[j] == '\t')
      tabs++;

  free(row->render);
  row->render = malloc(row->size + (tabs * (KILO_TAB_STOP - 1)) + 1);
  assert(row->render != NULL);

  int idx = 0;
  for (j = 0; j < row->size; j++) {
    if (row->chars[j] == '\t') {
      row->render[idx++] = ' ';
      while (idx % KILO_TAB_STOP != 0) {
        row->render[idx] = ' ';
        idx++;
      }
    } else {
      row->render[idx] = row->chars[j];
      idx++;
    }
  }
  row->render[idx] = '\0';
  row->render_size = idx;

  editorUpdateSyntax(row, row_idx);
}

void editorRowInsertChar(struct editorRow *row, int row_idx, int at, int c) {
  if (at < 0 || at > row->size) at = row->size;
  row->chars = realloc(row->chars, row->size + 2);
  assert(row->chars != NULL);
  memmove(&row->chars[at + 1], &row->chars[at], row->size - at + 1);
  row->size++;
  row->chars[at] = c;
  editorUpdateRow(row, row_idx);
  E.dirty++;
}

void editorRowAppendString(struct editorRow *row, int row_idx, char *s, size_t len) {
  row->chars = realloc(row->chars, row->size + len + 1);
  assert(row->chars != NULL);
  memcpy(&row->chars[row->size], s, len);
  row->size += len;
  row->chars[row->size] = '\0';
  editorUpdateRow(row, row_idx);
  E.dirty++;
}

void editorRowDelChar(struct editorRow *row, int row_idx, int at) {
  assert(at >= 0 && at <= row->size);
  memmove(&row->chars[at], &row->chars[at + 1], row->size - at);
  row->size--;
  editorUpdateRow(row, row_idx);
  E.dirty++;
}

void editorInsertRow(int at, char *s, size_t row_length) {
  assert(at >= 0 && at <= E.num_rows);

  E.rows = realloc(E.rows, sizeof(struct editorRow) * (E.num_rows + 1));
  assert(E.rows != NULL);
  struct editorRow *row = &E.rows[at];
  memmove(&E.rows[at + 1], row, sizeof(struct editorRow) * (E.num_rows - at));

  row->size = row_length;
  row->chars = malloc(row_length + 1);
  assert(row->chars != NULL);
  memcpy(row->chars, s, row_length);
  row->chars[row_length] = '\0';

  row->render_size = 0;
  row->render = NULL;
  row->hl = NULL;
  row->hl_open_comment = 0;
  editorUpdateRow(row, at);

  E.num_rows++;
  E.dirty++;
}

void editorFreeRow(struct editorRow *row) {
  free(row->render);
  free(row->chars);
  free(row->hl);
}

void editorDelRow(int at) {
  assert(at >= 0 && at < E.num_rows);
  editorFreeRow(&E.rows[at]);
  memmove(&E.rows[at], &E.rows[at + 1], sizeof(struct editorRow) * (E.num_rows - at - 1));
  E.num_rows--;
  E.dirty++;
}

/*** editor operations ***/

void editorInsertChar(int c) {
  if (E.cy == E.num_rows) {
    editorInsertRow(E.num_rows, "", 0);
  }
  editorRowInsertChar(&E.rows[E.cy], E.cy, E.cx, c);
  E.cx++;
}

void editorInsertNewline() {
  if (E.cx == 0) {
    editorInsertRow(E.cy, "", 0);
  } else {
    struct editorRow *row = &E.rows[E.cy];
    editorInsertRow(E.cy + 1, &row->chars[E.cx], row->size - E.cx);
    row = &E.rows[E.cy];
    row->size = E.cx;
    row->chars[row->size] = '\0';
    editorUpdateRow(row, E.cy);
  }
  E.cy++;
  E.cx = 0;
}

void editorDelChar() {
  if (E.cy == E.num_rows || (E.cx == 0 && E.cy == 0))
    return;

  struct editorRow *row = &E.rows[E.cy];
  if (E.cx > 0) {
    editorRowDelChar(row, E.cy, E.cx - 1);
    E.cx--;
  } else {
    E.cx = E.rows[E.cy - 1].size;
    editorRowAppendString(&E.rows[E.cy - 1], E.cy - 1, row->chars, row->size);
    editorDelRow(E.cy);
    E.cy--;
  }
}

/*** file i/o ***/

char *editorRowsToString(int *buffer_length) {
  int total_length = 0;
  int j;
  for (j = 0; j < E.num_rows; j++)
    total_length += E.rows[j].size + 1;
  *buffer_length = total_length;

  char *buffer = malloc(total_length);
  assert(buffer != NULL);
  char *p = buffer;
  for (j = 0; j < E.num_rows; j++) {
    memcpy(p, E.rows[j].chars, E.rows[j].size);
    p += E.rows[j].size;
    *p = '\n';
    p++;
  }

  return buffer;
}

void editorOpen(char *file_name) {
  free(E.file_name);
  E.file_name = strdup(file_name);

  editorSelectSyntaxHighlight();

  FILE *fp = fopen(file_name, "r");
  if (!fp)
    die("[ERROR] editorOpen fopen");

  char *line = NULL;
  size_t line_capacity = 0;
  ssize_t line_length;

  while((line_length = getline(&line, &line_capacity, fp)) != -1) {
    while (line_length > 0 && (line[line_length - 1] == '\n' || line[line_length - 1] == '\r'))
      line_length--;

    editorInsertRow(E.num_rows, line, line_length);
  }
  free(line);
  fclose(fp);
  E.dirty = 0;
}

void editorSave() {
  if (E.file_name == NULL) {
    E.file_name = editorPrompt("Save as: %s (ESC to cancel)", NULL);
    if (E.file_name == NULL) {
      editorSetStatusMessage("Save aborted");
      return;
    }
    editorSelectSyntaxHighlight();
  }

  int len;
  char *buffer = editorRowsToString(&len);

  int fd = open(E.file_name, O_RDWR | O_CREAT, 0644);
  if (fd != -1) {
    if (ftruncate(fd, len) != -1) {
      if (write(fd, buffer, len) == len) {
        close(fd);
        free(buffer);
        E.dirty = 0;
        editorSetStatusMessage("%d bytes written to disk", len);
        return;
      }
    }
    close(fd);
  }

  free(buffer);
  editorSetStatusMessage("Can't save! I/O error: %s", strerror(errno));
}

/** find ***/

void editorFindCallback(char *query, int key) {
  static int last_match = -1;
  static int direction = 1;

  static int saved_hl_line;
  static char *saved_hl = NULL;

  if (saved_hl) {
    memcpy(E.rows[saved_hl_line].hl, saved_hl, E.rows[saved_hl_line].render_size);
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
  int i;
  for (i = 0; i < E.num_rows; i++) {
    current += direction;
    if (current == -1) current = E.num_rows - 1;
    else if (current == E.num_rows) current = 0;

    struct editorRow *row = &E.rows[current];
    char *match = strstr(row->render, query);
    if (match) {
      last_match = current;
      E.cy = current;
      E.cx = editorRowRxToCx(row, match - row->render);
      E.row_offset = E.num_rows;

      saved_hl_line = current;
      saved_hl = malloc(row->render_size);
      assert(saved_hl != NULL);
      memcpy(saved_hl, row->hl, row->render_size);
      memset(&row->hl[match - row->render], HL_MATCH, strlen(query));
      break;
    }
  }
}

void editorFind() {
  int saved_cx = E.cx;
  int saved_cy = E.cy;
  int saved_col_offset = E.col_offset;
  int saved_row_offset = E.row_offset;

  char *query = editorPrompt("Search: %s (use ESC/Arrows/Enter)", editorFindCallback);

  if (query) {
    free(query);
  } else {
    E.cx = saved_cx;
    E.cy = saved_cy;
    E.col_offset = saved_col_offset;
    E.row_offset = saved_row_offset;
  }
}

/*** output ***/

void editorScroll() {
  E.rx = 0;
  if (E.cy < E.num_rows) {
    E.rx = editorRowCxToRx(&E.rows[E.cy], E.cx);
  }
  if (E.cy < E.row_offset) {
    E.row_offset = E.cy;
  } else if (E.cy >= E.row_offset + E.screen_rows) {
    E.row_offset = E.cy - E.screen_rows + 1;
  }
  
  if (E.rx < E.col_offset) {
    E.col_offset = E.rx;
  } else if (E.rx >= E.col_offset + E.screen_cols) {
    E.col_offset = E.rx - E.screen_cols + 1;
  }
}

void editorDrawRows(struct abuf *ab) {
  int y;
  for (y = 0; y < E.screen_rows; y++) {
    int row_idx = y + E.row_offset;
    if (row_idx >= E.num_rows) {
      if (E.num_rows == 0 && y == E.screen_rows / 3) {
        char welcome[80];
        int welcome_len =
          snprintf(welcome, sizeof(welcome), "Kilo editor -- version %s", KILO_VERSION);
        if (welcome_len > E.screen_cols)
          welcome_len = E.screen_cols;
        int padding = (E.screen_cols - welcome_len) / 2;
        if (padding) {
          abAppendLit(ab, "~");
          padding--;
        }
        while (padding--) abAppendLit(ab, " ");
        abAppend(ab, welcome, welcome_len);
      } else {
        abAppendLit(ab, "~");
      }
    } else {
      int len = E.rows[row_idx].render_size - E.col_offset;
      if (len < 0) len = 0;
      if (len > E.screen_cols) len = E.screen_cols;
      char *c = &E.rows[row_idx].render[E.col_offset];
      char *hl = &E.rows[row_idx].hl[E.col_offset];
      int current_color = -1;
      int j;
      for (j = 0; j < len; j++) {
        if (iscntrl(c[j])) {
          char sym = (c[j] <= 26) ? '@' + c[j] : '?';
          abAppendLit(ab, "\x1b[7m"); // Select Graphic Rendition - inverted colors
          abAppend(ab, &sym, 1);
          abAppendLit(ab, "\x1b[m"); // Select Graphic Rendition - clear all formatting
          if (current_color != -1) {
            char buf[16];
            int clen = snprintf(buf, sizeof(buf), "\x1b[%dm", current_color);
            abAppend(ab, buf, clen);
          }
        } else if (hl[j] == HL_NORMAL) {
          if (current_color != -1) {
            abAppendLit(ab, "\x1b[39m"); // Set Text Color - default
            current_color = -1;
          }
          abAppend(ab, &c[j], 1);
        } else {
          int color = editorSyntaxToColor(hl[j]);
          if (color != current_color) {
            current_color = color;
            char buf[16];
            int clen = snprintf(buf, sizeof(buf), "\x1b[%dm", color);
            abAppend(ab, buf, clen);
          }
          abAppend(ab, &c[j], 1);
        }
      }
      abAppendLit(ab, "\x1b[39m"); // Set Text Color - default
    }
    abAppendLit(ab, "\x1b[K\r\n"); // Erase In Line
  }
}

void editorDrawStatusBar(struct abuf *ab) {
  abAppendLit(ab, "\x1b[7m"); // Select Graphic Rendition - inverted colors
  char status[80], rstatus[80];
  int len = snprintf(status, sizeof(status), "%.20s - %d lines%s",
    E.file_name ? E.file_name : "[No File name]",
    E.num_rows,
    E.dirty ? " (modified)" : "");
  int rlen = snprintf(rstatus, sizeof(rstatus), "%s | %d:%d",
    E.syntax ? E.syntax->filetype : "no ft", E.cy + 1, E.cx + 1);
  if (len > E.screen_cols) len = E.screen_cols;
  abAppend(ab, status, len);
  while (len < E.screen_cols) {
    if (E.screen_cols - len == rlen) {
      abAppend(ab, rstatus, rlen);
      break;
    } else {
      abAppendLit(ab, " ");
      len++;
    }
  }
  abAppendLit(ab, "\x1b[m\r\n"); // Select Graphic Rendition - clear all formatting
}

void editorDrawMessageBar(struct abuf *ab) {
  abAppendLit(ab, "\x1b[K"); // Erase In Line
  if (E.statusmsg[0]) {
    int msglen = strlen(E.statusmsg);
    if (msglen > E.screen_cols)
      msglen = E.screen_cols;
    if (time(NULL) - E.statusmsg_time < 5) {
      abAppend(ab, E.statusmsg, msglen);
    } else {
      E.statusmsg[0] = '\0';
    }
  }
}

void editorRefreshScreen() {
  editorScroll();

  struct abuf ab = ABUF_INIT;

  abAppendLit(&ab, "\x1b[?25l\x1b[H"); // Reset Mode - draw cursor, Cursor Position

  editorDrawRows(&ab);
  editorDrawStatusBar(&ab);
  editorDrawMessageBar(&ab);

  char buf[32];
  snprintf(buf, sizeof(buf), "\x1b[%d;%dH", (E.cy - E.row_offset) + 1, (E.rx - E.col_offset) + 1); // Cursor Position
  abAppend(&ab, buf, strlen(buf));

  abAppendLit(&ab, "\x1b[?25h"); // Set Mode - draw cursor

  write(STDOUT_FILENO, ab.b, ab.len);
  abFree(&ab);
}

void editorSetStatusMessage(const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vsnprintf(E.statusmsg, sizeof(E.statusmsg), fmt, ap);
  va_end(ap);
  E.statusmsg_time = time(NULL);
}

/*** input ***/

char *editorPrompt(char *prompt, void (*callback)(char *, int)) {
  size_t buffer_size = 128;
  char *buffer = malloc(buffer_size);
  assert(buffer != NULL);

  size_t buffer_length = 0;
  buffer[0] = '\0';

  while(1) {
    editorSetStatusMessage(prompt, buffer);
    editorRefreshScreen();

    int c = editorReadKey();
    if (c == DEL_KEY || c == CTRL_KEY('h') || c == BACKSPACE ) {
      if (buffer_length != 0) {
        buffer_length--;
        buffer[buffer_length] = '\0';
      }
    } else if (c == '\x1b') {
      editorSetStatusMessage("");
      if (callback) callback(buffer, c);
      free(buffer);
      return NULL;
    } else if (c == '\r') {
      if (buffer_length != 0) {
        editorSetStatusMessage("");
        if (callback) callback(buffer, c);
        return buffer;
      }
    } else if (!iscntrl(c) && c < 128) {
      if (buffer_length == buffer_size - 1) {
        buffer_size *= 2;
        buffer = realloc(buffer, buffer_size);
        assert(buffer != NULL);
      }
      buffer[buffer_length] = c;
      buffer_length++;
      buffer[buffer_length] = '\0';
    }

    if (callback) callback(buffer, c);
  }
}

void editorMoveCursor(int key) {
  struct editorRow *row = (E.cy >= E.num_rows) ? NULL : &E.rows[E.cy];

  switch (key) {
    case ARROW_LEFT:
      if (E.cx != 0) {
        E.cx--;
      } else if (E.cy > 0) {
        E.cy--;
        E.cx = E.rows[E.cy].size;
      }
      break;
    case ARROW_RIGHT:
      if (row && E.cx < row->size) {
        E.cx++;
      } else if (row && E.cx == row->size) {
        E.cy++;
        E.cx = 0;
      }
      break;
    case ARROW_UP:
      if (E.cy != 0)
        E.cy--;
      break;
    case ARROW_DOWN:
      if (E.cy < E.num_rows)
        E.cy++;
      break;
  }

  row = (E.cy >= E.num_rows) ? NULL : &E.rows[E.cy];
  int row_length = row ? row->size : 0;
  if (E.cx > row_length) {
    E.cx = row_length;
  }
}

void editorProcessKeypress() {
  static int quit_times = KILO_QUIT_TIMES;
  int c = editorReadKey();

  switch (c) {
    case CTRL_KEY('q'):
      if (E.dirty && quit_times > 0) {
        editorSetStatusMessage("WARNING! File has unsaved changes. Press Ctrl-Q %d more times to quit.", quit_times);
        quit_times--;
        return;
      }
      write(STDOUT_FILENO, "\x1b[2J\x1b[0;0H", 10); // Erase In Display, Cursor Position
      exit(0);
      break;

    case CTRL_KEY('s'):
      editorSave();
      break;

    case CTRL_KEY('f'):
      editorFind();
      break;

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
          E.cy = E.row_offset;
        } else if (c == PAGE_DOWN) {
          E.cy = E.row_offset + E.screen_rows - 1;
          if (E.cy > E.num_rows)
            E.cy = E.num_rows;
        }
        int times = E.screen_rows;
        while (times--)
          editorMoveCursor(c == PAGE_UP ? ARROW_UP : ARROW_DOWN);
      }
      break;

    case HOME_KEY:
      E.cx = 0;
      break;
    case END_KEY:
      if (E.cy < E.num_rows)
        E.cx = E.rows[E.cy].size;
      break;
      
    case '\r': // enter
      editorInsertNewline();
      break;

    case BACKSPACE:
    case CTRL_KEY('h'):
    case DEL_KEY:
      if (c == DEL_KEY)
        editorMoveCursor(ARROW_RIGHT);
      editorDelChar();
      break;

    case CTRL_KEY('l'): // traditionally used to refresh screen
    case '\x1b': // escape
      break;

    default:
      editorInsertChar(c);
      break;
  }

  quit_times = KILO_QUIT_TIMES;
}

/*** init ***/

void initEditor() {
  DEBUG_LOG("initializing the editor");

  E.cx = 0;
  E.cy = 0;
  E.rx = 0;
  E.row_offset = 0;
  E.col_offset = 0;
  E.num_rows = 0;
  E.rows = NULL;
  E.dirty = 0;
  E.file_name = NULL;
  E.statusmsg[0] = '\0';
  E.statusmsg_time = 0;
  E.syntax = NULL;

  if (getWindowSize(&E.screen_rows, &E.screen_cols) == -1)
    die("[ERROR] initEditor getWindowSize");

  E.screen_rows -= 2; // reserve space for status and message bars
}

int main(int argc, char *argv[]) {
  #if DEBUG_FILE_LOG
    initLogger();
  #endif
  enableRawMode();
  initEditor();

  if (argc >= 2)
    editorOpen(argv[1]);

  editorSetStatusMessage("HELP: Ctrl-s = save | Ctrl-q = quit | Ctrl-f = find");

  while (1) {
    editorRefreshScreen();
    editorProcessKeypress();
  }

  return 0;
}
