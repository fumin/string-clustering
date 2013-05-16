#include <string.h>

int max(int a, int b) { return a > b ? a : b; };
int min(int a, int b) { return a > b ? b : a; };

int safe_strlen(const char const* s, const int max) {
  const char* p = s;
  while(p - s < max) {
    if (*p == '\0') {
      return p - s;
    }
    ++p;
  }

  return max;
};

#define MAX_LEN 128
#define CHAR_SET_SIZE 256 // Size of the ANSII character set

// The damerau-levenshtein distance between the strings a and b.
// Since this is a heavy compution, it's pointless to calculate this distance for
// strings that are too long.
// Here, we simply return MAX_LEN for strings with lengths that exceed MAX_LEN.
// Note that this implementation handles only English character strings.
int damerau_levenshtein(const char const* a, const char const* b) {
  int score[MAX_LEN+2][MAX_LEN+2];
  int sd[CHAR_SET_SIZE]; // This is actually used as a Map of char -> int

  int a_len = safe_strlen(a, MAX_LEN);
  int b_len = safe_strlen(b, MAX_LEN);
  int inf = a_len + b_len; // Infinity, the largest possible distance between these two strings
  int i, j, i1, j1, db = 0;

  if ( a_len == MAX_LEN || b_len == MAX_LEN ) { return MAX_LEN; }
  if ( a_len == 0 ) {
    if ( b_len == 0 ) { return 0; } else { return b_len; }
  }
  if ( b_len == 0 ) { return a_len; }

  // Initialize score matrix
  memset( score, 0, sizeof(score[0][0] * (MAX_LEN+2) * (MAX_LEN+2)) );
  for ( i = 0; i <= a_len; ++i ) {
    score[i+1][1] = i;
    score[i+1][0] = inf;
  }
  for ( j = 0; j <= b_len; ++j ) {
    score[1][j+1] = j;
    score[0][j+1] = inf;
  }

  // Initialize sd
  for ( i = 0; i != CHAR_SET_SIZE; ++i ) { sd[i] = 0; }

  // Main logic
  for ( i = 1; i <= a_len; ++i ) {
    db = 0;
    for ( j = 1; j <= b_len; ++j ) {
      i1 = sd[b[j-1]];
      j1 = db;
      if ( a[i-1] == b[j-1] ) {
        score[i+1][j+1] = score[i][j];
        db = j;
      } else {
        score[i+1][j+1] = min( score[i][j], min(score[i+1][j], score[i][j+1]) ) + 1;
      }

      score[i+1][j+1] = min( score[i+1][j+1], score[i1][j1] + (i - i1 - 1) + 1 + (j - j1 - 1));
    }

    sd[a[i-1]] = i;
  }

  return score[a_len+1][b_len+1];
};
