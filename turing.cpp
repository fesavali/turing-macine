#include stdio.h
#include stdlib.h
#include string.h
#include ctype.h
#include malloc.h
#include dos.h
#include time.h
#include stdafx.h


#define nFile 30    // file-name characters
#define nLine 350   // input-line characters
#define nDigits 2   // maximum digits in a decimal number
#define nBdigits 7  // maximum binary digits for decimal numbers
#define nStates 100 // maximum number of states
#define nAlpha 20   // maximum number of alphabet symbols

typedef struct { // structure for transition-matrix entries
    int q;       // next state
   char s,       // symbol to write on TM tape
        d;       // direction of motion of TM read/write head
} TMstruct, *TMstrPtr;

TMstrPtr TMrule[ nStates ][ nAlpha ]; // transition matrix of simulated TM

 char binary[ nStates ][ nBdigits + 1 ]; // binary-character string
                                         // representation of states

time_t tStart, tEnd; // start/end nSimSteps to calculate elapsed time

char EquivalentChar( char c )
{
   return (c == ' ') ? 'b' // encode spaces as 'b'
                     : c;
}// EquivalentChar

void PrintTMstring( TMstrPtr p )
{
   if ( p == NULL )
   {
      printf( "     /    " );
   }
   else
   {
      printf( " q%2d, %c,%c ",
              p->q, EquivalentChar( p->s ), p->d );
   }
}// PrintTMstring

typedef enum
        { number, lparen, rparen, comma,
          dash, _q, _b, _l, _r, _char, eol } SymbolType;

int      pos,  // current scan position in input line
         len,  // length of input line
      numVal,  // numeric value of symbolic number
     mStates,  // number of TM states
    mSymbols,  // number of TM symbols (including ' ')
       state,  // global index into TMrule[ state ][ ... ]
   nSimSteps;  // number of simulation steps

      FILE *fp;    // pointer to input file stream
SymbolType symbol; // type of symbol scanned

char    fileName[ nFile ], // input file name
         line[ nLine ],    // input line from file
     alphabet[ nLine ],    // TM alphabet
                 aChar;    // non-special single character

char *errorMsg[ 6 ]
     = { "'q'", "number", "'b' or non-special character",
         "'('", "'l', 'r' or '-'", "')'" };//users

void Error( int i )
{
   int j;

   for ( j = 0; j < pos - 1; ++j )
   {
      printf( " " );
   }
   printf( "^\nError %d: %s expected\n", i, errorMsg[ i ] );

   exit( 0 );
}// Error

void SkipComment()
{
   if ( pos < len && line[ pos ] == '{' )
   {
      while ( pos < len && line[ pos++ ] != '}' );
   }
}// SkipComment

void SkipBlanks()
{
   while ( pos < len && line[ pos ] == ' ' )
   {
      ++pos;//increment var
   }
}// SkipBlanks

void NextSymbol( int scanForNumOK = 1 )
{
   // pos < len

   char ch;
    int i;

   SkipBlanks();
   SkipComment();
   SkipBlanks();
   if ( pos < len )
   {
      ch = line[ pos++ ];
      if ( scanForNumOK && isdigit( (int)ch ) )
      {
         numVal = 0; i = 1;
         do {
            if ( i < nDigits )
            {
               numVal = numVal * 10 + ch - '0';
            }
            ch = line[ pos++ ];
         } while ( isdigit( (int)ch ) && pos < len );
         symbol = number;
      }//while case
      else
      {
         switch ( tolower( ch ) )
         {
            case '(': symbol = lparen;  break;
            case ')': symbol = rparen;  break;
            case ',': symbol = comma;   break;
            case '-': symbol = dash;    break;
            case 'q': symbol = _q;      break;
            case 'b': symbol = _b;      ch = ' '; break;
            case 'l': symbol = _l;      break;
            case 'r': symbol = _r;      break;
             default: symbol = _char;   break;
         }
         aChar = ch;
      }
   }//end switchcase
   else
   {
      symbol = eol;
   }
}// NextSymbol

int CharIndex( char a )
{
   int j;

   for ( j = 0; j < mSymbols; ++j )
   {
      if ( alphabet[ j ] == a )
      {
         break;
      }
   }
   return j;
}// CharIndex

void SkipComma()
{
   if ( symbol == comma )
   {
      NextSymbol( 0 );
   }
}// SkipComma

void TM_S( int * );

void TM_T()
{
       char inC, repC, dir;
        int nxtQ;
   TMstrPtr p;

   if ( symbol != eol )
   {
      if ( symbol == _b || symbol == _char )
      {
         inC = aChar;
         NextSymbol();
         if ( symbol == lparen )
         {
            NextSymbol();
            TM_S( &nxtQ );
            SkipComma();
            if ( symbol == _b || symbol == _char )
            {
               repC = aChar;
               NextSymbol();
               SkipComma();
               if ( symbol == _l || symbol == _r || symbol == dash )
               {
                  dir = aChar;
                  p = (TMstrPtr)malloc( sizeof( TMstruct ) );
                  p->q = nxtQ;
                  p->s = repC;
                  p->d = dir;
                  TMrule[ state ][ CharIndex( inC ) ] = p;
                  NextSymbol();
                  if ( symbol == rparen )
                  {
                     NextSymbol( 0 );
                     TM_T();
                  }
                  else Error( 5 );
               }
               else Error( 4 );
            }
            else Error( 2 );
         }
         else Error( 3 );
      }
      else Error( 2 );
   }
}// TM_T

void TM_S( int *var )
{
   if ( symbol == _q )
   {
      NextSymbol();
      if ( symbol == number )
      {
         *var = numVal;
         NextSymbol( 0 );
      }
      else Error( 1 );
   }
   else Error( 0 );
}// TM_S

void TM_P()
{
   TM_S( &state );
   TM_T();
}// TM_P

bool BlankLine( char line[], int len )
{
   int i;
   bool blank = true;

   for ( i = 0; i < len; ++i )
   {
      if ( line[ i ] != ' ' )
      {
         blank = false;
         break;
      }
   }
   return blank;
}// BlankLine

int GetLine( char line[] )
{
   int commentLine = 1;

   while ( commentLine && ( fgets( line, nLine, fp ) != NULL) )
   {
      len = strlen( line ) - 1;
      line[ len ] = '\0';
      if ( BlankLine( line, len ) )
      {
         printf( "" );
         continue;
      }
      if ( line[ 0 ] != ';' )
      {
         commentLine = 0;
      }
      else printf( "%s\n", line );
   }
   return len;
}// GetLine

void InputTM()
{
   int i, j;

   GetLine( line );
   mStates = atoi( line );
   mSymbols = GetLine( alphabet );
   for ( i = 0; i < nStates; ++i )
   {
      for ( j = 0; j < nAlpha; ++j )
      {
         TMrule[ i ][ j ] = NULL;
      }
   }
   printf( "\n" );
   while ( ( len = GetLine( line ) ) != 0 )
   {
     printf( "%s\n", line );
     if ( line[ 0 ] == '*' )
     {
        break;
     }
     pos = 0;
     if ( pos < len )
     {
        NextSymbol();
        TM_P();
     }
   }
}// InputTM

void ReportTM()
{
   int i, j;

   printf( "\nTuring machine: %s\n", fileName );

   printf( "%d states; \nalphabet: '%s', %d symbols\n",
           mStates, alphabet, mSymbols );

   printf( "\ntransition/action matrix:\n\n    " );
   for ( j = 0; j < mSymbols; ++j )
   {
      printf( "     %c    ", EquivalentChar( alphabet[ j ] ) );
   }
   printf( "\n\n" );
   for ( i = 0; i < mStates; ++i )
   {
      printf( "q%2d ", i );
      for ( j = 0; j < mSymbols; ++j )
      {
         PrintTMstring( TMrule[ i ][ j ] );
      }
      printf( "\n" );
   }
}// ReportTM

void PrintConfiguration( int q, char t[], int h )
{
    int i;
   char c;

   for ( i = 0; i < h; ++i )
   {
      printf( "%c", t[ i ] );
   }
   printf( "|q%d>", q );
   i = h;
   while ( i < len )
   {
      printf( "%c", t[ i++ ] );
   }
   while( (c = t[ i++ ]) != ' ' )
   {
      printf( "%c", c );
   }
   printf( "\n" );
}// PrintConfiguration

void ReportElapsedTime()
{
   time_t delta = tEnd - tStart;

   printf( "\nElapsed time: %d ", delta );
   if ( delta == 1 )
   {
      printf( "second" );
   }
   else
   {
      printf( "seconds" );
   }
}// ReportElapsedTime

typedef unsigned long int Dword;

void SimulateTM()
{
      char tape[ nLine ];
       int h, q, n, i, t;
  TMstrPtr p;
     Dword steps;

  time( &tStart );
  printf( "\nSimulating turing machine\n" );

  while ( ( n = GetLine( tape ) ) > 0 )
  {
     if ( tape[ 0 ] == '!' ) // terminator for input cases
     {
        break;
     }
     printf( "\n" );
     for ( i = n; i < nLine - 1; ++i )
     {
        tape[ i ] = ' ';
     }
     tape[ nLine - 1 ] = '\0';
     t = h = q = 0;
     steps = 0L;
     while ( q >= 0 )
     {
        PrintConfiguration( q, tape, h );
        p = TMrule[ q ][ CharIndex( tape[ h ] ) ];
        if ( p != NULL )
        {
           ++steps;
           q = p->q;
           tape[ h ] = p->s;
           if ( p->d == '-' )
           {
              break;
           }
           h += ( tolower( p->d ) == 'r' ) ? 1 : -1;
           if ( h < 0 || h == nLine )
           {
              printf( "illegal head position: %d\n", h );
              q = -1;
           }
           if ( nSimSteps > 0 )
           {
              ++t;
              if ( t > nSimSteps )
              {
                 break;
              }
           }
        }
        else
        {
           break;
        }
     }
     PrintConfiguration( q, tape, h );
     printf( "\nInput %saccepted\n",
             ( q == mStates -1 ) ? "" : "not " );
     printf( "%lu simulation steps\n", steps );
  }
  fclose( fp );
  time( &tEnd );
  ReportElapsedTime();
}// SimulateTM

int Log2( int n )
{
   int nBits = 1, Pof2 = 2;

   while ( Pof2 < n )
   {
      Pof2 <<= 1;
      ++nBits;
   }
   return nBits;
}// Log2

void GenerateBinaryStates()
{
   int i, j, k, nBits = Log2( mStates );

   printf( "\nGenerateBinaryStates: %d states ==> %d bits\n\n",
           mStates, nBits );
   for ( i = 0; i < mStates; ++i )
   {
      printf( "i: %2d ", i );
      binary[ i ][ nBits ] = '\0';
      k = i;
      for ( j = nBits - 1; -1 < j; --j )
      {
         binary[ i ][ j ] = (k & 1) ? '1' : '0';
         k >>= 1;
      }
      printf( "%s\n", binary[ i ] );
   }
   printf( "\n\n" );
}// GenerateBinaryStates

void PrintQuintuple( int i, int j, TMstrPtr p )
{
   if ( p != NULL )
   {
      printf( "x%s%c%s%c%c",
               binary[ i ], alphabet[ j ],
               binary[ p->q ], p->s, (p->d == 'L') ? '0' : '1' );
   }
}// PrintQuintuple

void DescribeTM()
{
   int i, j;

   printf( "\nTM description for LCCP UTM:\n\n" );
   GenerateBinaryStates();
   printf( "y%s0", binary[ 0 ] ); // Initial state of TM and symbol "scanned"
   //printf( "\n" );
   for ( i = 0; i < mStates; ++i )
   {
      for ( j = 0; j < mSymbols; ++j )
      {
         PrintQuintuple( i, j, TMrule[ i ][ j ] );
      }
   }
   printf( "y0\n" );
}// DescribeTM

void FreeMemory()
{
     int i, j;
   TMstrPtr p;

   for ( i = 0; i < mStates; ++i )
   {
      for ( j = 0; j < mSymbols; ++j )
      {
         p = TMrule[ i ][ j ];
         if ( p != NULL )
         {
            free( p );
         }
      }
   }
}// FreeMemory

int _tmain(int argc, _TCHAR* argv[])
{
   printf( "input file? " );
   gets( fileName );
   if ( ( fp = fopen( fileName, "r" ) ) == NULL )
   {
      printf( "\nunable to open file '%s'\n", fileName );
   }
   else
   {
      printf( "%s\n", fileName );
      printf( "number of simulation steps [0 = indefinite]? " );
      scanf( "%d", &nSimSteps );
      printf( "%d\n", nSimSteps );
      InputTM();
      ReportTM();
      SimulateTM();
      DescribeTM();
      FreeMemory();
   }
   return 0;
}// _tmain

