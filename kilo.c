/*** includes ***/

#define _DEFAULT_SOURCE
#define _BSD_SOURCE
#define _GNU_SOURCE

#include <ctype.h>
#include <errno.h>
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

/*** data ***/

struct editorRow {
  int size;
  int render_size;
  char *chars;
  char *render;
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
  struct editorRow *row;
  int dirty;
  char *file_name;
  char statusmsg[80];
  time_t statusmsg_time;
  struct termios orig_termios;
};

struct editorConfig E;

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

void disableRawMode() {
  if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &E.orig_termios) == -1)
    die("[ERROR] disableRawMode tcsetattr");
}

void enableRawMode() {
  if (tcgetattr(STDIN_FILENO, &E.orig_termios) == -1)
    die("[ERROR] enableRawMode tcgetattr");
  atexit(disableRawMode);

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
  char buf[32];
  unsigned int i = 0;

  if (write(STDOUT_FILENO, "\x1b[6n", 4) != 4) // Device Status Report - cursor position
    return -1;

  while (i < sizeof(buf) - 1) {
    if (read(STDIN_FILENO, &buf[i], 1) != 1)
      break;
    if (buf[i] == 'R')
      break;
    i++;
  }
  buf[i] = '\0';

  if (buf[0] != '\x1b' || buf[1] != '[')
    return -1;

  if (sscanf(&buf[2], "%d;%d", rows, cols) != 2)
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

void editorUpdateRow(struct editorRow *row) {
  int tabs = 0;
  int j;
  for (j = 0; j < row->size; j++)
    if (row->chars[j] == '\t')
      tabs++;

  free(row->render);
  row->render = malloc(row->size + (tabs * (KILO_TAB_STOP - 1)) + 1);

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
}

void editorRowInsertChar(struct editorRow *row, int at, int c) {
  if (at < 0 || at > row->size) at = row->size;
  row->chars = realloc(row->chars, row->size + 2);
  memmove(&row->chars[at + 1], &row->chars[at], row->size - at + 1);
  row->size++;
  row->chars[at] = c;
  editorUpdateRow(row);
  E.dirty++;
}

void editorRowAppendString(struct editorRow *row, char *s, size_t len) {
  row->chars = realloc(row->chars, row->size + len + 1);
  memcpy(&row->chars[row->size], s, len);
  row->size += len;
  row->chars[row->size] = '\0';
  editorUpdateRow(row);
  E.dirty++;
}

void editorRowDelChar(struct editorRow *row, int at) {
  if (at < 0 || at > row->size) return;
  memmove(&row->chars[at], &row->chars[at + 1], row->size - at);
  row->size--;
  editorUpdateRow(row);
  E.dirty++;
}

void editorInsertRow(int at, char *s, size_t len) {
  if (at < 0 || at > E.num_rows)
    return;

  E.row = realloc(E.row, sizeof(struct editorRow) * (E.num_rows + 1));
  memmove(&E.row[at + 1], &E.row[at], sizeof(struct editorRow) * (E.num_rows - at));

  E.row[at].size = len;
  E.row[at].chars = malloc(len + 1);
  memcpy(E.row[at].chars, s, len);
  E.row[at].chars[len] = '\0';

  E.row[at].render_size = 0;
  E.row[at].render = NULL;
  editorUpdateRow(&E.row[at]);

  E.num_rows++;
  E.dirty++;
}

void editorFreeRow(struct editorRow *row) {
  free(row->render);
  free(row->chars);
}

void editorDelRow(int at) {
  if (at < 0 || at >= E.num_rows)
    return;
  editorFreeRow(&E.row[at]);
  memmove(&E.row[at], &E.row[at + 1], sizeof(struct editorRow) * (E.num_rows - at - 1));
  E.num_rows--;
  E.dirty++;
}

/*** editor operations ***/

void editorInsertChar(int c) {
  if (E.cy == E.num_rows) {
    editorInsertRow(E.num_rows, "", 0);
  }
  editorRowInsertChar(&E.row[E.cy], E.cx, c);
  E.cx++;
}

void editorInsertNewline() {
  if (E.cx == 0) {
    editorInsertRow(E.cy, "", 0);
  } else {
    struct editorRow *row = &E.row[E.cy];
    editorInsertRow(E.cy + 1, &row->chars[E.cx], row->size - E.cx);
    row = &E.row[E.cy];
    row->size = E.cx;
    row->chars[row->size] = '\0';
    editorUpdateRow(row);
  }
  E.cy++;
  E.cx = 0;
}

void editorDelChar() {
  if (E.cy == E.num_rows || (E.cx == 0 && E.cy == 0))
    return;

  struct editorRow *row = &E.row[E.cy];
  if (E.cx > 0) {
    editorRowDelChar(row, E.cx - 1);
    E.cx--;
  } else {
    E.cx = E.row[E.cy - 1].size;
    editorRowAppendString(&E.row[E.cy - 1], row->chars, row->size);
    editorDelRow(E.cy);
    E.cy--;
  }
}

/*** file i/o ***/

char *editorRowsToString(int *buffer_length) {
  int total_length = 0;
  int j;
  for (j = 0; j < E.num_rows; j++)
    total_length += E.row[j].size + 1;
  *buffer_length = total_length;

  char *buffer = malloc(total_length);
  char *p = buffer;
  for (j = 0; j < E.num_rows; j++) {
    memcpy(p, E.row[j].chars, E.row[j].size);
    p += E.row[j].size;
    *p = '\n';
    p++;
  }

  return buffer;
}

void editorOpen(char *file_name) {
  free(E.file_name);
  E.file_name = strdup(file_name);

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

    struct editorRow *row = &E.row[current];
    char *match = strstr(row->render, query);
    if (match) {
      last_match = current;
      E.cy = current;
      E.cx = editorRowRxToCx(row, match - row->render);
      E.row_offset = E.num_rows;
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

/*** output ***/

void editorScroll() {
  E.rx = 0;
  if (E.cy < E.num_rows) {
    E.rx = editorRowCxToRx(&E.row[E.cy], E.cx);
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
    int file_row = y + E.row_offset;
    if (file_row >= E.num_rows) {
      if (E.num_rows == 0 && y == E.screen_rows / 3) {
        char welcome[80];
        int welcome_len =
          snprintf(welcome, sizeof(welcome), "Kilo editor -- version %s", KILO_VERSION);
        if (welcome_len > E.screen_cols)
          welcome_len = E.screen_cols;
        int padding = (E.screen_cols - welcome_len) / 2;
        if (padding) {
          abAppend(ab, "~", 1);
          padding--;
        }
        while (padding--) abAppend(ab, " ", 1);
        abAppend(ab, welcome, welcome_len);
      } else {
        abAppend(ab, "~", 1);
      }
    } else {
      int len = E.row[file_row].render_size - E.col_offset;
      if (len < 0) len = 0;
      if (len > E.screen_cols) len = E.screen_cols;
      abAppend(ab, E.row[file_row].render + E.col_offset, len);
    }
    abAppend(ab, "\x1b[K\r\n", 5); // Erase In Line
  }
}

void editorDrawStatusBar(struct abuf *ab) {
  abAppend(ab, "\x1b[7m", 4); // Select Graphic Rendition - inverted colors
  char status[80], rstatus[80];
  int len = snprintf(status, sizeof(status), "%.20s - %d lines%s",
    E.file_name ? E.file_name : "[No File name]",
    E.num_rows,
    E.dirty ? " (modified)" : "");
  int rlen = snprintf(rstatus, sizeof(rstatus), "%d:%d", E.cy + 1, E.cx + 1);
  if (len > E.screen_cols) len = E.screen_cols;
  abAppend(ab, status, len);
  while (len < E.screen_cols) {
    if (E.screen_cols - len == rlen) {
      abAppend(ab, rstatus, rlen);
      break;
    } else {
      abAppend(ab, " ", 1);
      len++;
    }
  }
  abAppend(ab, "\x1b[m\r\n", 5); // Select Graphic Rendition - clear
}

void editorDrawMessageBar(struct abuf *ab) {
  abAppend(ab, "\x1b[K", 3); // Erase In Line
  int msglen = strlen(E.statusmsg);
  if (msglen > E.screen_cols)
    msglen = E.screen_cols;
  if (msglen && time(NULL) - E.statusmsg_time < 5)
    abAppend(ab, E.statusmsg, msglen);
}

void editorRefreshScreen() {
  editorScroll();

  struct abuf ab = ABUF_INIT;

  abAppend(&ab, "\x1b[?25l\x1b[H", 9); // Reset Mode - draw cursor, Cursor Position

  editorDrawRows(&ab);
  editorDrawStatusBar(&ab);
  editorDrawMessageBar(&ab);

  char buf[32];
  snprintf(buf, sizeof(buf), "\x1b[%d;%dH", (E.cy - E.row_offset) + 1, (E.rx - E.col_offset) + 1); // Cursor Position
  abAppend(&ab, buf, strlen(buf));

  abAppend(&ab, "\x1b[?25h", 6); // Set Mode - draw cursor

  write(STDOUT_FILENO, ab.b, ab.len);
  abFree(&ab);
}

void editorSetStatusMessage(const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vsnprintf(E.statusmsg, sizeof(E.statusmsg), fmt, ap); // takes care of calling va_arg based on statically known fmt
  va_end(ap);
  E.statusmsg_time = time(NULL);
}

/*** input ***/

char *editorPrompt(char *prompt, void (*callback)(char *, int)) {
  size_t buffer_size = 128;
  char *buffer = malloc(buffer_size);

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
      }
      buffer[buffer_length] = c;
      buffer_length++;
      buffer[buffer_length] = '\0';
    }

    if (callback) callback(buffer, c);
  }
}

void editorMoveCursor(int key) {
  struct editorRow *row = (E.cy >= E.num_rows) ? NULL : &E.row[E.cy];

  switch (key) {
    case ARROW_LEFT:
      if (E.cx != 0) {
        E.cx--;
      } else if (E.cy > 0) {
        E.cy--;
        E.cx = E.row[E.cy].size;
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

  row = (E.cy >= E.num_rows) ? NULL : &E.row[E.cy];
  int row_length = row ? row-> size : 0;
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
        E.cx = E.row[E.cy].size;
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
  E.cx = 0;
  E.cy = 0;
  E.rx = 0;
  E.row_offset = 0;
  E.col_offset = 0;
  E.num_rows = 0;
  E.row = NULL;
  E.dirty = 0;
  E.file_name = NULL;
  E.statusmsg[0] = '\0';
  E.statusmsg_time = 0;

  if (getWindowSize(&E.screen_rows, &E.screen_cols) == -1)
    die("[ERROR] initEditor getWindowSize");
  E.screen_rows -= 2;
}

int main(int argc, char *argv[]) {
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
