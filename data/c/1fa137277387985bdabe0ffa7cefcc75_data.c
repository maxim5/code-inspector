/*
 *	DATA.C
 *      E.Werner, Tenjiku Shogi
 *      based on
 *	Tom Kerrigan's Simple Chess Program (TSCP)
 *
 *	Copyright 1997 Tom Kerrigan
 */


#include "defs.h"


/* the board representation */
int influences[2][NUMSQUARES];
int color[NUMSQUARES];  /* LIGHT, DARK, or EMPTY */
int piece[NUMSQUARES];  /* PAWN, KNIGHT, SILVER, LANCE, GOLD, KING, or EMPTY */
int suicide[NUMSQUARES];
int side;  /* the side to move */
int xside;  /* the side not to move */
int last_capture;  /* the number of moves since a capture, to handle repetitions*/
int ply;  /* the half-move that we're on */

/* the number of pieces each player has got */
int pieces_count[2] = { 78, 78 };
int fire_demons[2] = { 2, 2};

/* gen_dat is some memory for move lists that are created by the move
   generators. The move list for ply n starts at gen_begin[n] of gen_dat.
   gen_end[n] is right after the move list. */
gen_t gen_dat[MOVE_STACK];
int gen_begin[HIST_STACK], gen_end[HIST_STACK];

/* the history heuristic array (used for move ordering) */
int history[NUMSQUARES][NUMSQUARES];

/* we need an array of hist_t's so we can take back the
   moves we make */
hist_t hist_dat[HIST_STACK];

/* the engine will search for max_time milliseconds or until it finishes
   searching max_depth ply. */
long max_time;
int max_depth;

/* the time when the engine starts searching, and when it should stop */
long start_time;
long stop_time;

long nodes;  /* the number of nodes we've searched */

/* a "triangular" PV array */
move pv[HIST_STACK][HIST_STACK];
int pv_length[HIST_STACK];
BOOL follow_pv;

/* undo_dat is some memory to save the moves from the game
   (not just from analysis) so the user can take back moves. */
hist_t undo_dat[UNDO_STACK];
int undos;


/* Now we have the mailbox array, so called because it looks like a
   mailbox, at least according to Bob Hyatt. This is useful when we
   need to figure out what pieces can go where. Let's say we have a
   rook on square a4 (32) and we want to know if it can move one
   square to the left. We subtract 1, and we get 31 (h5). The rook
   obviously can't move to h5, but we don't know that without doing
   a lot of annoying work. Sooooo, what we do is figure out a4's
   mailbox number, which is 61. Then we subtract 1 from 61 (60) and
   see what mailbox[60] is. In this case, it's -1, so it's out of
   bounds and we can forget it. You can see how mailbox[] is used
   in attack() in board.c. */

int mailbox[400] = {
	 -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
	 -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
	 -1, -1,  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, -1, -1,
	 -1, -1, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, -1, -1,
	 -1, -1, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, -1, -1,
	 -1, -1, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, -1, -1,
	 -1, -1, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, -1, -1,
	 -1, -1, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, -1, -1,
	 -1, -1, 96, 97, 98, 99,100,101,102,103,104,105,106,107,108,109,110,111, -1, -1,
	 -1, -1,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127, -1, -1,
	 -1, -1,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143, -1, -1,
	 -1, -1,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159, -1, -1,
	 -1, -1,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175, -1, -1,
	 -1, -1,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191, -1, -1,
	 -1, -1,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207, -1, -1,
	 -1, -1,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223, -1, -1,
	 -1, -1,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239, -1, -1,
	 -1, -1,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255, -1, -1,
	 -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
	 -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1
};

int mailbox256[NUMSQUARES] = {
   42,  43,  44,  45,  46,  47,  48,  49,  50,  51,  52,  53,  54,  55,  56,  57, 
   62,  63,  64,  65,  66,  67,  68,  69,  70,  71,  72,  73,  74,  75,  76,  77, 
   82,  83,  84,  85,  86,  87,  88,  89,  90,  91,  92,  93,  94,  95,  96,  97,
  102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117,
  122, 123, 124, 125, 126, 127, 128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 
  142, 143, 144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157,
  162, 163, 164, 165, 166, 167, 168, 169, 170, 171, 172, 173, 174, 175, 176, 177,
  182, 183, 184, 185, 186, 187, 188, 189, 190, 191, 192, 193, 194, 195, 196, 197,
  202, 203, 204, 205, 206, 207, 208, 209, 210, 211, 212, 213, 214, 215, 216, 217,
  222, 223, 224, 225, 226, 227, 228, 229, 230, 231, 232, 233, 234, 235, 236, 237,
  242, 243, 244, 245, 246, 247, 248, 249, 250, 251, 252, 253, 254, 255, 256, 257,
  262, 263, 264, 265, 266, 267, 268, 269, 270, 271, 272, 273, 274, 275, 276, 277,
  282, 283, 284, 285, 286, 287, 288, 289, 290, 291, 292, 293, 294, 295, 296, 297,
  302, 303, 304, 305, 306, 307, 308, 309, 310, 311, 312, 313, 314, 315, 316, 317,
  322, 323, 324, 325, 326, 327, 328, 329, 330, 331, 332, 333, 334, 335, 336, 337,
  342, 343, 344, 345, 346, 347, 348, 349, 350, 351, 352, 353, 354, 355, 356, 357
};


int offsets[PIECE_TYPES] = {
  /* Pawn, King, DE, G, S, C, I, FL, N, L, RC, ChS, BT, Ky, Ph, FK, SM, SSd, VM, VSd, R, Ln */
	1, 8, 7, 6, 5, 4, 3, 6, 2, 1, 2, 8, 7, 8, 8, 8, 4, 4, 4, 4, 4, 0,
	/* B, DH, DK, HF, SEg, WB, Dg, BGn, RGn, GGn, FEg, LHk, VGn, FiD */
	4, 8, 8, 8, 8, 8, 3, 4, 4, 8, 8, 4, 4, 6,
	/* CP, FO, FS, FBo, HT, MGn, +P, W, WH */
	8, 6, 8, 6, 6, 3, 6, 4, 4,
	/* FK, SM, SSd, VM, VSd, R, Ln, B, DH, DK, HF, SEg, WB, BGn, RGn */
	8, 4, 4, 4, 4, 4, 0, 4, 8, 8, 8, 8, 8, 4, 4,8
};

int promotion[PIECE_TYPES] = {
  TOKIN, 0, CROWN_PRINCE, PROOK, PVERTICAL_MOVER, PSIDE_MOVER, 
  PVERTICAL_SOLDIER, PBISHOP, PSIDE_SOLDIER, WHITE_HORSE, 
  WHALE, HEAVENLY_TETRARCHS, FLYING_STAG, PLION, PFREE_KING, FREE_EAGLE, FREE_BOAR, 
  PWATER_BUFFALO, FLYING_OX, PCHARIOT_SOLDIER, PDRAGON_KING, LION_HAWK, 
  PDRAGON_HORSE, PHORNED_FALCON, PSOARING_EAGLE, PBISHOP_GENERAL, PROOK_GENERAL, FIRE_DEMON, MULTI_GENERAL, VICE_GENERAL, 
  GREAT_GENERAL, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0
};

move_type move_types[PIECE_TYPES][9] = {
  /* first eight are ofset directions, 9th is lion/igui/ */
  { step, none, none, none, none, none, none, none, none },       /* Pawn */
  { step, step, step, step, step, step, step, step, none }, /* King */
  { step, step, step, step, step, step, step, none, none }, /* Drunken Elephant */
  { step, step, step, step, step, step, none, none, none }, /* Gold */
  { step, step, step, step, step, none, none, none, none }, /* Silver */
  { step, step, step, step, none, none, none,none, none },  /* Copper */
  { step, step, step, none, none, none, none,none, none },  /* Iron */
  { step, step, step, step, step, step, none, none, none }, /* Ferocious Leopard */
  { step, step, none, none, none, none, none, none, none },     /* Knight */
  { slide, none, none, none, none, none, none, none, none },    /* Lance */
  { slide, slide, none, none, none, none, none, none, none },   /* Reverse Chariot */
  { slide, slide, slide, two_steps, two_steps, slide, slide, slide, none },  /* Chariot Soldier */
  { step, step, step, step, step, step, step, step, none },  /* BT */
  { step, step, step, step, step, step, step, step, none },  /* Ky */
  { step, step, step, step, step, step, step, step, none },  /* Ph */
  { slide, slide, slide, slide, slide, slide, slide, slide, none },  /* FK */
  { slide, slide, step, step, none, none, none, none, none },  /* SM */
  { slide, slide, two_steps, step, none, none, none, none, none },  /* SSd */
  { slide, slide, step, step, none, none, none, none, none },  /* VM */
  { slide, slide, two_steps, two_steps, none, none, none, none, none },  /* VSd */
  { slide, slide, slide, slide, none, none, none, none, none },  /* R */
  { none, none, none, none, none, none, none, none, lion}, /* Ln */
  { slide, slide, slide, slide, none, none, none, none, none },  /* B */
  { slide, slide, slide, slide, step, step, step, step, none },  /* DH */
  { slide, slide, slide, slide, step, step, step, step, none },  /* DK */
  { slide, lion, slide, slide, slide, slide, slide, slide, none },  /* HF */
  { lion, slide, lion, slide, slide, slide, slide, slide, none },  /* SEg */
  { slide, slide, slide, two_steps, two_steps, slide, slide, slide, none },/*WB*/
  { step, step, step, none, none, none, none, none, none },  /* Dg */
  { jumpslide, jumpslide, jumpslide, jumpslide, none, none, none, none, none },  /* BGn */
  { jumpslide, jumpslide, jumpslide, jumpslide, none, none, none, none, none },  /* RGn */
  { jumpslide, jumpslide, jumpslide, jumpslide, jumpslide, jumpslide, jumpslide, jumpslide, none },  /* GGn */
  { slide, free_eagle, slide, free_eagle, free_eagle, slide, free_eagle, slide, none },  /* FEg */
  { slide, slide, slide, slide, none, none, none, none, area2 },  /* LHk */
  { jumpslide, jumpslide, jumpslide, jumpslide, none, none, none, none, area3 },  /* VGn */
  { slide, slide, slide, slide, slide, slide, none, none, area3 },  /* FiD */
  { step, step, step, step, step, step, step, step, none }, /* CP */
  { slide, slide, slide, slide, slide, slide, none, none, none },  /* FO */
  { step, slide, step, step, step, step, slide, step, none }, /* FS */
  { slide, slide, slide, slide, slide, slide, none, none, none },  /* FBo */
  { slide, tetrarch, slide, slide, tetrarch, slide, none, none, igui_capture },  /* HT */
  { slide, slide, slide, none, none, none, none, none, none },  /* MGn */
  { step, step, step, step, step, step, none, none, none }, /* Tokin */
  { slide, slide, slide, slide, none, none, none,none, none },  /* Whale */
  { slide, slide, slide, slide, none, none, none,none, none },  /* White Horse */
  { slide, slide, slide, slide, slide, slide, slide, slide, none },  /* FK */
  { slide, slide, step, step, none, none, none, none, none },  /* SM */
  { slide, slide, two_steps, step, none, none, none, none, none },  /* SSd */
  { slide, slide, step, step, none, none, none, none, none },  /* VM */
  { slide, slide, two_steps, two_steps, none, none, none, none, none },  /* VSd */
  { slide, slide, slide, slide, none, none, none, none, none },  /* R */
  { none, none, none, none, none, none, none, none, lion}, /* Ln */
  { slide, slide, slide, slide, none, none, none, none, none },  /* B */
  { slide, slide, slide, slide, step, step, step, step, none },  /* DH */
  { slide, slide, slide, slide, step, step, step, step, none },  /* DK */
  { slide, lion, slide, slide, slide, slide, slide, slide, none },  /* HF */
  { lion, slide, lion, slide, slide, slide, slide, slide, none },  /* SEg */
  { slide, slide, slide, two_steps, two_steps, slide, slide, slide, none },/*WB*/
  { jumpslide, jumpslide, jumpslide, jumpslide, none, none, none, none, none },  /* BGn */
  { jumpslide, jumpslide, jumpslide, jumpslide, none, none, none, none, none }  /* RGn */
};

int lion_jumps[16] = {
  -42, -41, -40, -39, -38, -22, -2, -18, 18, 2, 22, 38, 39, 40, 41, 42
};

int lion_single_steps[8] = { -21, -20, -19, -1, 19, 20, 21, 1 };

int offset[2][PIECE_TYPES][8] = {
  /* sente's pieces */
{ { -20,   0,   0,  0,  0,  0,  0, 0 },       /* Pawn */
  { -21, -20, -19, -1,  1, 19, 20, 21 },  /* King */
  { -21, -20, -19, -1,   1, 19, 21, 0 },  /* Drunken Elephant */
  { -21, -20, -19, -1,   1, 20,  0, 0 },  /* Gold */
  { -21, -19,  19, 21, -20,  0,  0, 0 },   /* Silver */
  { -21, -20, -19, 20,   0,  0,  0, 0 },  /* Copper */
  { -21, -20, -19, 0, 0, 0, 0,0 },  /* Iron */
  { -21, -19, 19, 21, -20, 20, 0, 0 },   /* Ferocious Leopard */
  { -41, -39, 0, 0, 0, 0, 0, 0 },     /* Knight */
  { -20, 0, 0, 0, 0, 0, 0, 0 },       /* Lance */
  { -20, 20, 0, 0, 0, 0, 0, 0 },       /* Reverse Chariot */
  { -21, -20, -19, -1, 1, 19, 20, 21, },  /* Chariot Soldier */
  { -21,  20, -19, -1, 21, 19, 1, 0 },  /* Blind Tiger */
  { -21, -40, -19, -2, 21, 19, 2, 40},  /* Kylin */
  { -42, -20, -38, -1, 42, 38, 1, 20 },  /* Phoenix */
  { -21, -20, -19, -1, 1, 19, 20, 21 },  /* Free King */
  { -1, 1, 20, -20, 0, 0, 0, 0 },  /* SM */
  { -1, 1, -20, 20, 0, 0, 0, 0 },  /* SSd */
  { -20, 20, -1, 1, 0, 0, 0, 0 },  /* VM */
  { -20, 20, -1, 1, 0, 0, 0, 0 },  /* VSd */
  { -1, 1, 20, -20, 0, 0, 0, 0 },  /* Rook */
  { 0, 0, 0, 0, 0, 0, 0, 0 }, /* Lion */
  { -21, -19, 21, 19, 0, 0, 0, 0 },  /* B */
  { -21, -19, 21, 19, 20, -20, -1, 1 },  /* DH */
  { 20, -20, -1, 1, -21, -19, 21, 19 },  /* DK */
  { -21, -20, -19, -1, 1, 19, 20, 21 },  /* HF */
  { -21, -20, -19, -1, 1, 19, 20, 21 },  /* SEg */
  { -21, -1, -19, -20, 20, 19, 1, 21 },  /* WB */
  { 21, -20,  19, 0, 0, 0, 0, 0 },  /* Dg*/
  { -21, -19, 21, 19, 0, 0, 0, 0 },  /* BGn */
  { -1, 1, 20, -20, 0, 0, 0, 0 },  /* RGn */
  { -21, -20, -19, -1, 1, 19, 20, 21 },  /* GGn */
  { -21, -20, -19, -1, 1, 19, 20, 21 },  /* FEg */
  { -21, -19, 21, 19, 0, 0, 0, 0 },  /* LHk */
  { -21, -19, 21, 19, 0, 0, 0, 0 },  /* VGn */
  { -21, -20, -19, 21, 20, 19, 0, 0 },  /* FiD */
  { -21, -20, -19, -1,  1, 19, 20, 21 },  /* CP */
  { -21, -20, -19, 21, 20, 19, 0, 0 },  /* FO */
  { -21, -20, -19, -1,  1, 19, 20, 21 },  /* FS */
  { -21, -1, -19, 21, 1, 19, 0, 0 },  /* FBo */
  { -21, -1, -19, 21, 1, 19, 0, 0 },  /* HT */
  { 21, -20, 19, 0, 0, 0, 0, 0 },  /* MGn */
  { 21, -20, 19, -1,   1, 20,  0, 0 },  /* Tokin */
  { -21, -20, -19, 20, 0, 0, 0, 0 },  /* W */
  { 21, -20, 19, 20, 0, 0, 0, 0 },  /* WH */
  { -21, -20, -19, -1, 1, 19, 20, 21 },  /* Free King */
  { -1, 1, 20, -20, 0, 0, 0, 0 },  /* SM */
  { -1, 1, -20, 20, 0, 0, 0, 0 },  /* SSd */
  { -20, 20, -1, 1, 0, 0, 0, 0 },  /* VM */
  { -20, 20, -1, 1, 0, 0, 0, 0 },  /* VSd */
  { -1, 1, 20, -20, 0, 0, 0, 0 },  /* Rook */
  { 0, 0, 0, 0, 0, 0, 0, 0 }, /* Lion */
  { -21, -19, 21, 19, 0, 0, 0, 0 },  /* B */
  { -21, -19, 21, 19, 20, -20, -1, 1 },  /* DH */
  { 20, -20, -1, 1, -21, -19, 21, 19 },  /* DK */
  { -21, 20, -19, -1, 1, 19, -20, 21 },  /* HF */
  { 21, -20, 19, -1, 1, -19, 20, -21 },  /* SEg */
  { -21, -1, -19, -20, 20, 19, 1, 21 },  /* WB */
  { -21, -19, 21, 19, 0, 0, 0, 0 },  /* BGn */
  { -1, 1, 20, -20, 0, 0, 0, 0 },  /* RGn */
 }, /* gote's pieces */
 { { 20, 0, 0, 0, 0, 0, 0, 0 },       /* Pawn */
  { -21, -20, -19, -1, 1, 19, 20, 21 },  /* King */
  { -21, -19, -1, 1, 19, 20, 21, 0 },  /* Drunken Elephant */
  {  21, 20, 19, 1, -1, -20, 0, 0 },  /* Gold */
  { -21, -19, 19, 21, 20, 0, 0, 0 },   /* Silver */
  {  21, 20, 19, -20, 0, 0, 0, 0 },  /* Copper */
  {  21, 20, 19, 0, 0, 0, 0, 0 },  /* Iron */
  { -21, -19, 19, 21, 20, -20, 0, 0 },   /* Ferocious Leopard */
  {  41, 39, 0, 0, 0, 0, 0, 0 },     /* Knight */
  {  20, 0, 0, 0, 0, 0, 0, 0 },       /* Lance */
  { -20, 20, 0, 0, 0, 0, 0, 0 },       /* Reverse Chariot */
  { -21, -20, -19, -1, 1, 19, 20, 21 },  /* Chariot Soldier */
  { -21, -20, -19, -1, 21, 19, 1, 0 },  /* Blind Tiger */
  { -21, -40, -19, -2, 21, 19, 2, 40 },  /* Kylin */
  { -42, -20, -38, -1, 42, 38, 1, 20 },  /* Phoenix */
  { -21, -20, -19, -1, 1, 19, 20, 21 },  /* Free King */
  { -1, 1, 20, -20, 0, 0, 0, 0 },  /* SM */
  { -1, 1, 20, -20, 0, 0, 0, 0 },  /* SSd */
  { 20, -20, -1, 1, 0, 0, 0, 0 },  /* VM */
  { 20, -20, 1, -1, 0, 0, 0, 0 },  /* VSd */
  { -1, 1, 20, -20, 0, 0, 0, 0 },  /* Rook */
  { 0, 0, 0, 0, 0, 0, 0, 0 }, /* Lion */
  { -21, -19, 21, 19, 0, 0, 0, 0 },  /* B */
  { -21, -19, 21, 19, 20, -20, -1, 1 },  /* DH */
   { 20, -20, -1, 1, -21, -19, 21, 19 },  /* DK */
  { -21, 20, -19, -1, 1, 19, -20, 21 },  /* HF */
  { 21, -20, 19, -1, 1, -19, 20, -21 },  /* SEg */
  { -21, -1, -19, -20, 20, 19, 1, 21 },  /* WB */
  { -21, 20, -19, 0, 0, 0, 0, 0 },  /* Dg*/
  { -21, -19, 21, 19, 0, 0, 0, 0 },  /* BGn */
  { -1, 1, 20, -20, 0, 0, 0, 0 },  /* RGn */
  { -21, -20, -19, -1, 1, 19, 20, 21 },  /* GGn */
  { -21, -20, -19, -1, 1, 19, 20, 21 },  /* FEg */
  { -21, -19, 21, 19, 0, 0, 0, 0 },  /* LHk */
  { -21, -19, 21, 19, 0, 0, 0, 0 },  /* VGn */
  { -21, -20, -19, 21, 20, 19, 0, 0 },  /* FiD */
  { -21, -20, -19, -1,  1, 19, 20, 21 },  /* CP */
  { -21, -20, -19, 21, 20, 19, 0, 0 },  /* FO */
  { -21, -20, -19, -1,  1, 19, 20, 21 },  /* FS */
  { -21, -1, -19, 21, 1, 19, 0, 0 },  /* FBo */
  { -21, -1, -19, 21, 1, 19, 0, 0 },  /* HT */
  { 21, -20, 19, 0, 0, 0, 0, 0 },  /* MGn */
  { 21, -20, 19, -1,   1, 20,  0, 0 },  /* Tokin */
  { -21, -20, -19, 20, 0, 0, 0, 0 },  /* W */
  {  21, -20,  19, 20, 0, 0, 0, 0 },  /* WH */
  { -21, -20, -19, -1, 1, 19, 20, 21 },  /* Free King */
  { -1, 1, 20, -20, 0, 0, 0, 0 },  /* SM */
  { -1, 1, 20, -20, 0, 0, 0, 0 },  /* SSd */
  { 20, -20,-1, 1, 0, 0, 0, 0 },  /* VM */
  { 20, -20, -1, 1, 0, 0 },  /* VSd */
  { -1, 1, 20, -20, 0, 0, 0, 0 },  /* Rook */
  { 0, 0, 0, 0, 0, 0, 0, 0 }, /* Lion */
  { -21, -19, 21, 19, 0, 0, 0, 0 },  /* B */
  { -21, -19, 21, 19, 20, -20, -1, 1 },  /* DH */
  { 20, -20, -1, 1, -21, -19, 21, 19 },  /* DK */
  { -21, 20, -19, -1, 1, 19, -20, 21 },  /* HF */
  { 21, -20, 19, -1, 1, -19, 20, -21 },  /* SEg */
  { -21, -1, -19, -20, 20, 19, 1, 21 },  /* WB */
  { -21, -19, 21, 19, 0, 0, 0, 0 },  /* BGn */
  { -1, 1, 20, -20, 0, 0, 0, 0 },  /* RGn */
  { -21, -20, -19, -1, 1, 19, 20, 21 }  /* Chariot Soldier */
} };

int can_promote_zone[2][PIECE_TYPES] = {
  /* Pawn, King, DE, G, S, C, I, FL, N, L, RC, ChS, BT, Ky, Ph, FK, SM, SSd, VM, VSd, R, Ln, B, DH, DK, HF, SEg, WB, Dg */
{ 79,  0, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }, 
{ 175, 0, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }};

int must_promote_zone[2][PIECE_TYPES] = {
  /* Pawn, King, DE, G, S, C, I, FL, N, L, RC, ChS, BT, Ky, Ph, FK, SM, SSd, VM, VSd, R, Ln, HF, SEg, WB, Dg */
    {  16, 0 , 0, 0, 0, 0, 16, 0, 32, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }, 
    { 239, 0, 0, 0, 0, 0, 239, 0, 223, 239, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }};

/* the piece letters, for print_board() */
unsigned char *piece_string[PIECE_TYPES] = {
	"P  ", "K  ", "DE ", "G  ", "S  ", "C  ", "I  ", "FL ", "N  ", "L  ", "RC ", "ChS", "BT ",
	"Ky ", "Ph ", "FK ", "SM ", "SSd", "VM ", "VSd", "R  ", "Ln ", "B  ",
	"DH ", "DK ", "HF ", "SEg", "WB ", "Dg ", "BGn", "RGn", "GGn", "FEg", "LHk", "VGn", "FiD",
	"CP ", "FO ", "FS ", "FBo", "HT ", "MGn", "+P ", "W  ", "WH ", 
	/* promoted and unpromoted */
	 "fk ", "sm ", "ssd", "vm ", "vsd", "r  ", "ln ", "b  ",
	"dh ", "dk ", "hf ", "seg", "wb ", "bgn", "rgn", "chs"
};


/* the initial board state */

int init_color[NUMSQUARES] = {
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	1, 61, 1, 1, 61, 1, 1, 1, 1, 1, 1, 61, 1, 1, 61, 1,
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 1,	1, 1, 1, 1, 1, 1, 1, 1,
	61, 61, 61, 61, 1, 61, 61, 61, 61, 61, 61, 1, 61, 61, 61, 61,
	61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61,
	61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61,
	61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61,
	61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61,
	61, 61, 61, 61, 0, 61, 61, 61, 61, 61, 61, 0, 61, 61, 61, 61,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 61, 0, 0, 61, 0, 0, 0, 0, 0, 0, 61, 0, 0, 61, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
};

int init_piece[NUMSQUARES] = {
	9, 8, 7, 6, 5, 4, 3, 2, 1, 3, 4, 5, 6, 7, 8, 9,
	10, 61, 11, 11, 61, 12, 14, 15, 21, 13, 12, 61, 11, 11, 61, 10,
	17, 19, 22, 23, 24, 27, 35, 32, 33, 35, 27, 24, 23, 22, 19, 17,
	16, 18, 20, 25, 26, 29, 30, 34, 31, 30, 29, 26, 25, 20, 18, 16,
	0, 0, 0, 0, 0, 0, 0, 0,	0, 0, 0, 0, 0, 0, 0, 0,
	61, 61, 61, 61, 28, 61, 61, 61, 61, 61, 61, 28, 61, 61, 61, 61,
	61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61,
	61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61,
	61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61,
	61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61,
	61, 61, 61, 61, 28, 61, 61, 61, 61, 61, 61, 28, 61, 61, 61, 61,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	16, 18, 20, 25, 26, 29, 30, 31, 34, 30, 29, 26, 25, 20, 18, 16,
	17, 19, 22, 23, 24, 27, 35, 33, 32, 35, 27, 24, 23, 22, 19, 17,
	10, 61, 11, 11, 61, 12, 13, 21, 15, 14, 12, 61, 11, 11, 61, 10,
	9, 8, 7, 6, 5, 4, 3, 1, 2, 3, 4, 5, 6, 7, 8, 9
};


int depth_adj[PIECE_TYPES] = {
  200, 300, 300, 300,
  300, 300, 300, 300,
  200, 200, 200, 220, 250,
  250, 250, 200, 300, 300,
  250, 250, 200, 200, 200,
  200, 300, 150, 150, 100,
  200, 150, 100, 100, /* FiD */
  200, 200, 200, 
  200, 200, 200,
  200, 200, 200,
  /* promoted and unpromoted (hardly likely) */
  200, 200, 200,
  200, 200, 200,
  200, 200, 200,
  200, 200, 200,
  200, 200,
  200, 200, 200
}; 


/* dummy values for testing
int depth_adj[PIECE_TYPES] = {
  100, 100, 100, 100,
  100, 100, 100, 100,
  100, 100, 100, 100, 100,
  100, 100, 100, 100, 100,
  100, 100, 100, 100, 100,
  100, 100, 100, 100, 100,
  100, 150, 100, 100,
  100, 100, 100, 
  100, 100, 100,
  100, 100, 100,
  * promoted and unpromoted (hardly likely) *
  100, 100, 100,
  100, 100, 100,
  100, 100, 100,
  100, 100, 100,
  100, 100,
  100, 100, 100
};
*/

#ifdef EZ
unsigned char *piece_name[PIECE_TYPES+1] = {
  "Pawn (Fuhyo)", "King (Gyokusho)", "Drunk Elephant", "Gold General",
  "Silver General", "Copper General", "Iron General", "Ferocious Leopard",
  "Knight", "Lance", "Reverse Chariot", "Chariot Soldier", "Blind Tiger",
  "Kylin", "Phoenix", "Free King", "Side Mover", "Side Soldier", 
  "Vertical Mover", "Vertical Soldier", "Rook", "Lion", "Bishop",
  "Dragon Horse", "Dragon King", "Horned Falcon", "Soaring Eagle", 
  "Water Buffalo", "Dog", "Bishop General", "Rook General", "Great General",
  "Free Eagle", "Lion Hawk", "Vice General", "Fire Demon",
  "Crown Prince (+DE)", "Flying Ox (+VM)", "Flying Stag (+BT)", 
  "Free Boar (+SM)", "Heavenly Tetrarchs (+ChS)", "Multi General (+Dg)", 
  "Tokin (+P)", "Whale (+RC)", "White Horse (+L)", 
	/* promoted and unpromoted */
  "Free King (+Ph)", "Side Mover (+C)", "Side Solder (+N)", 
  "Verical Mover (+S)", "Vertical Soldier (+I)", "Rook (+G)", 
  "Lion (+Ky)", "Bishop (+FL)",
  "Dragon Horse (+B)", "Dragon King (+R)", "Horned Falcon (+DH)",
  "Soaring Eagle (+DK)", "Water Buffalo (+SSd) ", 
  "Bishop General (+HF)", "Rook General (+SEg)", "Chariot Soldier (+VSd)", "(empty) "
};

#endif
