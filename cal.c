#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#define MAXLEN 256
#define TBLSIZE 65535

typedef enum {UNKNOWN, END, INT, ID, ADDSUB, MULDIV, ASSIGN, LPAREN, RPAREN, ANDOR, ENDFILE} TokenSet;
static TokenSet getToken(void);
static TokenSet lookahead = UNKNOWN;
static char lexeme[MAXLEN];

typedef struct {
    char name[MAXLEN];
    int val;
} Symbol;

Symbol table[TBLSIZE];

int sbcount = 3;
int lefts, ra;

int r[10];

TokenSet getToken(void)
{
    int i;
    char c;

    while ( (c = fgetc(stdin)) == ' ' || c== '\t' );

    if(c == EOF) {
        return ENDFILE;
    } else if(isdigit(c)) {
        lexeme[0] = c;
        c = fgetc(stdin);
        i = 1;
        while (isdigit(c) && i<MAXLEN) {
            lexeme[i] = c;
            ++i;
            c = fgetc(stdin);
        }
        ungetc(c, stdin);
        lexeme[i] = '\0';
        return INT;
    } else if (c == '+' || c == '-') {
        lexeme[0] = c;
        lexeme[1] = '\0';
        return ADDSUB;
    } else if (c == '*' || c == '/') {
        lexeme[0] = c;
        lexeme[1] = '\0';
        return MULDIV;
    } else if(c == '&' || c == '^' || c == '|'){
        lexeme[0] = c;
        lexeme[1] = '\0';
        return ANDOR;
    } else if (c == '\n') {
        lexeme[0] = '\0';
        return END;
    } else if (c == '=') {
        strcpy(lexeme, "=");
        return ASSIGN;
    } else if (c == '(') {
        strcpy(lexeme, "(");
        return LPAREN;
    } else if (c == ')') {
        strcpy(lexeme, ")");
        return RPAREN;
    } else if (isalpha(c) || c == '_') {
        lexeme[0] = c;
        c = fgetc(stdin);
        i = 1;
        while (isalpha(c) || isdigit(c) || c == '_') {
            lexeme[i] = c;
            ++i;
            c = fgetc(stdin);
        }
        ungetc(c, stdin);
        lexeme[i] = '\0';
        return ID;
    } else {
        return UNKNOWN;
    }
}

void advance(void)
{
    lookahead = getToken();
}

int match(TokenSet token)
{
    if (lookahead == UNKNOWN) advance();
    return token == lookahead;
}

char* getLexeme(void)
{
    return lexeme;
}

typedef struct _Node {
    char lexeme[MAXLEN];
    TokenSet token;
    int val;
    struct _Node *left, *right;
} BTNode;

void statement(void);
BTNode* expr(void);
BTNode* term(void);
BTNode* factor(void);
int getval(void);
int setval(char*, int);

typedef enum {MISPAREN, NOTNUMID, NOTFOUND, RUNOUT, NAN} ErrorType;
void error(ErrorType errorNum);

/* create a node without any child */
BTNode* makeNode(TokenSet tok, const char *lexe)
{
    BTNode *node = (BTNode*) malloc(sizeof(BTNode));
    strcpy(node->lexeme, lexe);
    node->token= tok;
    node->val = 0;
    node->left = NULL;
    node->right = NULL;
    return node;
}

/* print a tree by pre-order */
void printPrefix(BTNode *root)
{
    if (root != NULL) {
        printf("%s ", root->lexeme);
        printPrefix(root->left);
        printPrefix(root->right);
    }
}

/* clean a tree */
void freeTree(BTNode *root)
{
    if (root!=NULL) {
        freeTree(root->left);
        freeTree(root->right);
        free(root);
    }
}

int sym(const char nam[MAXLEN]){
    for(int i = 0; i < TBLSIZE; i++){
        if(strcmp(nam,table[i].name) == 0) return i*4;
    }
    return 0;
}

int last(){
    for(int i = 7 ; i >=0 ; i--){
        if(r[i] == 1) return i;
    }
    return 0;
}

int blank(){
    for(int i = 0 ; i < 8 ; i++){
        if(r[i] == 0) return i;
    }
    return 0;
}

int evaluateTree(BTNode *root, int islft)
{
    if(!ra){
        printf("EXIT 1");
        exit(0);
    }
    int retval = 0, lv, rv;
    if (root != NULL)
    {
        switch (root->token)
        {
            case INT:
                if(islft){
                    retval = root->val;
                    int k = blank();
                    r[k] = 1;
                    printf("MOV r%d %d\n", k, root->val);
                }
                break;
            case ID:
                if(islft){
                    retval = root->val;
                    int m = blank();
                    r[m] = 1;
                    printf("MOV r%d [%d]\n", m, sym(root->lexeme));
                }
                break;
            case ANDOR:
            case ASSIGN:
            case ADDSUB:
            case MULDIV:
                if(root->left->token == ID && root->token == ASSIGN)lv = evaluateTree(root->left,0);
                else lv = evaluateTree(root->left,1);
                rv = evaluateTree(root->right,1);
                if (strcmp(root->lexeme, "+") == 0)
                {
                    int k = last();
                    r[k] = 0;
                    int m = last();
                    printf("ADD r%d r%d\n", m, k);
                    retval = lv + rv;
                }
                else if (strcmp(root->lexeme, "-") == 0)
                {
                    int k = last();
                    r[k] = 0;
                    int m = last();
                    printf("SUB r%d r%d\n", m, k);
                    retval = lv - rv;
                }
                else if (strcmp(root->lexeme, "*") == 0)
                {
                    int k = last();
                    r[k] = 0;
                    int m = last();
                    printf("MUL r%d r%d\n", m, k);
                    retval = lv * rv;
                }
                else if (strcmp(root->lexeme, "/") == 0) {
                        int k = last();
                        r[k] = 0;
                        int m = last();
                        printf("DIV r%d r%d\n", m, k);
                        retval = 1;
                }
                else if (strcmp(root->lexeme, "=") == 0)
                {
                    retval = setval(root->left->lexeme, rv);
                    int k = last();
                    r[k] = 0;
                    printf("MOV [%d] r%d\n", sym(root->left->lexeme), k);
                }
                else if (strcmp(root->lexeme, "&") == 0)
                {
                    int k = last();
                    r[k] = 0;
                    int m = last();
                    printf("AND r%d r%d\n", m, k);
                    retval = lv & rv;
                }
                else if (strcmp(root->lexeme, "|") == 0)
                {
                    int k = last();
                    r[k] = 0;
                    int m = last();
                    printf("OR r%d r%d\n", m, k);
                    retval = lv | rv;
                }
                else if (strcmp(root->lexeme, "^") == 0)
                {
                    int k = last();
                    r[k] = 0;
                    int m = last();
                    printf("XOR r%d r%d\n", m, k);
                    retval = lv ^ rv;
                }
                break;
            default:
                retval = 0;
				break;
        }
    }
    return retval;
}

int getval(void)
{
    int i, retval = 0, found;

    if (match(INT))
    {
        retval = atoi(getLexeme());
    } else if (match(ID))
    {
        i = 0;
        found = 0;
        retval = 0;
        while (i<sbcount && !found) {
            if (strcmp(getLexeme(), table[i].name)==0) {
                retval = table[i].val;
                found = 1;
                break;
            } else {
                i++;
            }
        }
        if (!found) {
            if (sbcount < TBLSIZE) {
                strcpy(table[sbcount].name, getLexeme());
                table[sbcount].val = 0;
                sbcount++;
            } else {
                error(RUNOUT);
            }
        }
    }
    return retval;
}

int setval(char *str, int val)
{
    int i, retval = 0;
    i = 0;
    while (i<sbcount) {
        if (strcmp(str, table[i].name)==0) {
            table[i].val = val;
            retval = val;
            break;
        } else {
            i++;
        }
    }
    return retval;
}

BTNode* expr(void)
{
    BTNode *retp, *left;
    retp = left = term();
    while (match(ADDSUB)||match(ANDOR)) {
        if(match(ADDSUB)) retp = makeNode(ADDSUB, getLexeme());
        else retp = makeNode(ANDOR, getLexeme());
        advance();
        retp->right = term();
        retp->left = left;
        left = retp;
    }
    return retp;
}

BTNode* term(void)
{
    BTNode *retp, *left;
    retp = left = factor();
    while (match(MULDIV)) {
        retp = makeNode(MULDIV, getLexeme());
        advance();
        retp->right = factor();
        retp->left = left;
        left = retp;
    }
    return retp;
}

BTNode* factor(void)
{
    BTNode* retp = NULL;
    char tmpstr[MAXLEN];

    if (match(INT)) {
        if(!ra){
            printf("EXIT 1");
            exit(0);
        }
        else{
            retp =  makeNode(INT, getLexeme());
            retp->val = getval();
            advance();
        }
    } else if (match(ID)) {
        if(!ra && lefts){
            printf("EXIT 1");
            exit(0);
        }
        lefts=1;

        BTNode* left = makeNode(ID, getLexeme());
        left->val = getval();
        strcpy(tmpstr, getLexeme());
        advance();
        if (match(ASSIGN)) {
            ra = 1;
            retp = makeNode(ASSIGN, getLexeme());
            advance();
            retp->right = expr();
            retp->left = left;
        } else {
            retp = left;
        }
    } else if (match(ADDSUB)) {
        strcpy(tmpstr, getLexeme());
        advance();
        if (match(ID) || match(INT)) {
            retp = makeNode(ADDSUB, tmpstr);
            if (match(ID))
                retp->right = makeNode(ID, getLexeme());
            else
                retp->right = makeNode(INT, getLexeme());
            retp->right->val = getval();
            retp->left = makeNode(INT, "0");
            retp->left->val = 0;
            advance();
        } else {
            error(NOTNUMID);
        }
    } else if (match(LPAREN)) {
        advance();
        retp = expr();
        if (match(RPAREN)) {
            advance();
        } else {
            error(MISPAREN);
        }
    } else {
        error(NOTNUMID);
    }
    return retp;
}

void error(ErrorType errorNum)
{
    printf("EXIT 1\n");

    switch (errorNum) {
        case MISPAREN:
            fprintf(stderr, "Mismatched parenthesis\n");
            break;
        case NOTNUMID:
            fprintf(stderr, "Number or identifier expected\n");
            break;
        case NOTFOUND:
            fprintf(stderr, "%s not defined\n", getLexeme());
            break;
        case RUNOUT:
            fprintf(stderr, "Out of memory\n");
            break;
        case NAN:
            fprintf(stderr, "Not a number\n");
    }
    exit(0);
}

void statement(void)
{
    BTNode* retp;
    if (match(END)) {
        advance();
    }
    else if(match(ENDFILE)){
        for(int i=0 ; i<3 ; i++){
            printf("MOV r%d [%d]\n", i, i*4);
        }
        printf("EXIT 0\n");
        exit(0);
    }
    else {
        retp = expr();
        if (match(END)) {
            evaluateTree(retp,1);
            freeTree(retp);
            advance();
        }
        else error(1);
    }
}

void initialize(void){
    strcpy(table[0].name,"x");
    strcpy(table[1].name,"y");
    strcpy(table[2].name,"z");
}

int main(void)
{
    initialize();

    while (1) {
        lefts = 0; ra = 0;
        memset(r, 0, 10);
        statement();
    }
    return 0;
}

