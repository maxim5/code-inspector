/*
 *	DATA.C
 *      E.Werner, Tenjiku Shogi
 *      based on
 *	Tom Kerrigan's Simple Chess Program (TSCP)
 *
 *	Copyright 1997 Tom Kerrigan
 */


#include "defs.h"
#include <wchar.h>

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
tenjikumove pv[HIST_STACK][HIST_STACK];
int pv_length[HIST_STACK];
BOOL follow_pv;

/* undo_dat is some memory to save the moves from the game
   (not just from analysis) so the user can take back moves. */
hist_t undo_dat[UNDO_STACK];
hist_t redo_dat[REDO_STACK];
int undos;
int redos;


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
	4, 8, 8, 8, 8, 8, 3, 4, 4, 8, 8, 4, 4, 8,
	/* CP, FO, FS, FBo, HT, MGn, +P, W, WH */
	8, 6, 8, 6, 
	8, 
	3, 6, 4, 4,
	/* FK, SM, SSd, VM, VSd, R, Ln, B, DH, DK, HF, SEg, WB, BGn, RGn */
	8, 4, 4, 4, 4, 4, 0, 4, 8, 8, 8, 8, 8, 4, 4, 8,
	/* pfree_eagle, plion_hawk, pfire_demon, pvice_general, pgreat_general */
	8, 4, 6, 4, 8
};
/* pfree_eagle, plion_hawk, pfire_demon, pvice_general, pgreat_general */

int promotion[PIECE_TYPES] = {
  TOKIN, 0, CROWN_PRINCE, PROOK, PVERTICAL_MOVER, PSIDE_MOVER, 
  PVERTICAL_SOLDIER, PBISHOP, PSIDE_SOLDIER, WHITE_HORSE, 
  WHALE, HEAVENLY_TETRARCHS, FLYING_STAG, PLION, PFREE_KING, PFREE_EAGLE, FREE_BOAR, 
  PWATER_BUFFALO, FLYING_OX, PCHARIOT_SOLDIER, PDRAGON_KING, PLION_HAWK, 
  PDRAGON_HORSE, PHORNED_FALCON, PSOARING_EAGLE, PBISHOP_GENERAL, PROOK_GENERAL, PFIRE_DEMON, 
  MULTI_GENERAL, PVICE_GENERAL, 
  PGREAT_GENERAL, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0
};

/*
  int demote[PIECE_TYPES] = {
  PAWN, KING, DRUNK_ELEPHANT, GOLD, SILVER, COPPER,
*/

move_type move_types[PIECE_TYPES][9] = {
  /* first eight are offset directions, 9th is lion/igui/ */
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
  { slide, step, two_steps, two_steps, slide, none, none, none, none },  /* VSd */
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
#ifdef edo_style_FEg
  {  free_eagle,  free_eagle,  free_eagle,  free_eagle,  free_eagle, free_eagle, free_eagle, free_eagle, none },  /* FEg */
#else
  { slide, free_eagle, slide, free_eagle, free_eagle, slide, free_eagle, slide, none },  /* FEg */
#endif
  { slide, slide, slide, slide, none, none, none, none, area2 },  /* LHk */
  { jumpslide, jumpslide, jumpslide, jumpslide, none, none, none, none, area3 },  /* VGn */
  { slide, slide, slide, slide, slide, slide, none, none, area3 },  /* FiD */
  { step, step, step, step, step, step, step, step, none }, /* CP */
  { slide, slide, slide, slide, slide, slide, none, none, none },  /* FO */
  { step, slide, step, step, step, step, slide, step, none }, /* FS */
  { slide, slide, slide, slide, slide, slide, none, none, none },  /* FBo */
#ifdef japanese_HT
 { htslide, tetrarch, htslide, htslide, tetrarch, htslide, htslide, htslide, igui_capture },  /* HT */
#else 
 { htslide, tetrarch, htslide, htslide, tetrarch, htslide, none, none, igui_capture },  /* HT */
#endif
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
  { jumpslide, jumpslide, jumpslide, jumpslide, none, none, none, none, none },  /* RGn */
  { slide, slide, slide, two_steps, two_steps, slide, slide, slide, none },  /* Chariot Soldier */
/* pfree_eagle, plion_hawk, pfire_demon, pvice_general, pgreat_general */
  { slide, free_eagle, slide, free_eagle, free_eagle, slide, free_eagle, slide, none },  /* pFEg */
  { slide, slide, slide, slide, none, none, none, none, area2 },  /* pLHk */
  { slide, slide, slide, slide, slide, slide, none, none, area3 },  /* pFiD */
  { jumpslide, jumpslide, jumpslide, jumpslide, none, none, none, none, area3 },  /* pVGn */
  { jumpslide, jumpslide, jumpslide, jumpslide, jumpslide, jumpslide, jumpslide, jumpslide, none }  /* pGGn */
};

int lion_jumps[16] = {
  -42, -41, -40, -39, -38, -22, -2, -18, 18, 2, 22, 38, 39, 40, 41, 42
};

int lion_single_steps[8] = { -21, -20, -19, -1, 19, 20, 21, 1 };

int offset[2][PIECE_TYPES][8] = {
  /* gote's pieces */
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
#ifdef japanese_FiD
  { -21, -1, -19, 21, 1, 19, 0, 0 },  /* FiD */
#else
  { -21, -20, -19, 21, 20, 19, 0, 0 },  /* FiD */
#endif
  { -21, -20, -19, -1,  1, 19, 20, 21 },  /* CP */
  { -21, -20, -19, 21, 20, 19, 0, 0 },  /* FO */
  { -21, -20, -19, -1,  1, 19, 20, 21 },  /* FS */
  { -21, -1, -19, 21, 1, 19, 0, 0 },  /* FBo */
#ifdef japanese_HT
  { -21, -1, -19, 21, 1, 19, 20, -20 },  /* HT */
#else
  { -21, -1, -19, 21, 1, 19, 0, 0 },  /* HT */
#endif
  { 21, -20, 19, 0, 0, 0, 0, 0 },  /* MGn */
  { 21, -20, 19, -1,   1, 20,  0, 0 },  /* Tokin */
  { -21, -20, -19, 20, 0, 0, 0, 0 },  /* W */
  { 21, -20, 19, 20, 0, 0, 0, 0 },  /* WH */
  { -21, -20, -19, -1, 1, 19, 20, 21 },  /* Free King */
  { -1, 1, 20, -20, 0, 0, 0, 0 },  /* SM */
  { -1, 1, -20, 20, 0, 0, 0, 0 },  /* SSd */
  { -20, 20, -1, 1, 0, 0, 0, 0 },  /* VM */
  { 20, -20, -1, 1, 0, 0, 0, 0 },  /* VSd */
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
   { -21, -20, -19, -1, 1, 19, 20, 21 },  /* pChariot Soldier */
/* pfree_eagle, plion_hawk, pfire_demon, pvice_general, pgreat_general */
  { -21, -20, -19, -1, 1, 19, 20, 21 },  /* FEg */
  { -21, -19, 21, 19, 0, 0, 0, 0 },  /* LHk */
  { -21, -20, -19, 21, 20, 19, 0, 0 },  /* FiD */
  { -21, -19, 21, 19, 0, 0, 0, 0 },  /* VGn */
  { -21, -20, -19, -1, 1, 19, 20, 21 }  /* GGn */
 }, /* sente's pieces */
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
#ifdef japanese_FiD
  { -21, -1, -19, 21, 1, 19, 0, 0 },  /* FiD */
#else
  { -21, -20, -19, 21, 20, 19, 0, 0 },  /* FiD */
#endif
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
   { -21, -20, -19, -1, 1, 19, 20, 21 },  /* pChariot Soldier */
/* pfree_eagle, plion_hawk, pfire_demon, pvice_general, pgreat_general */
  { -21, -20, -19, -1, 1, 19, 20, 21 },  /* FEg */
  { -21, -19, 21, 19, 0, 0, 0, 0 },  /* LHk */
  { -21, -20, -19, 21, 20, 19, 0, 0 },  /* FiD */
  { -21, -19, 21, 19, 0, 0, 0, 0 },  /* VGn */
  { -21, -20, -19, -1, 1, 19, 20, 21 },  /* GGn */
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
	" P ", " K ", "DE ", " G ", " S ", " C ", " I ", "FL ", " N ", " L ", "RC ", "ChS", "BT ",
	"Ky ", "Ph ", "FK ", "SM ", "SSd", "VM ", "VSd", " R ", "Ln ", " B ",
	"DH ", "DK ", "HF ", "SEg", "WB ", "Dg ", "BGn", "RGn", "GGn", "FEg", "LHk", "VGn", "FiD",
	"\e[31mCP\e[30m ", "\e[31mFO\e[30m ", "\e[31mFS\e[30m ", "\e[31mFBo\e[30m", "\e[31mHT\e[30m ", "\e[31mMGn\e[30m", "\e[31m+P\e[30m", " \e[31mW\e[30m ", "\e[31mWH\e[30m ", 
	/* promoted and unpromoted */
	 "\e[31mFK \e[30m", "\e[31mSM\e[30m ", "\e[31mSSd\e[30m", "\e[31mVM\e[30m ", "\e[31mVSD\e[30m", " \e[31mR\e[30m ", "\e[31mLn \e[30m", " \e[31mB\e[30m ",
	"\e[31mDH\e[30m ", "\e[31mDK\e[30m ", "\e[31mHF\e[30m ", "\e[31mSEg\e[30m", "\e[31mWB \e[0m", "\e[31mBGn\e[30m", "\e[31mRGn\e[30m", "\e[31mChS\e[30m",
 "\e[31mFEg\e[30m", "\e[31mLHk\e[0m", "\e[31mFiD\e[30m", "\e[31mVGn\e[30m", "\e[31mGGn\e[30m",
};

unsigned char *unpadded_piece_string[PIECE_TYPES] = {
	"P", "K", "DE", "G", "S", "C", "I", "FL", "N", "L", "RC", "ChS", "BT",
	"Ky", "Ph", "FK", "SM", "SSd", "VM", "VSd", "R", "Ln", "B",
	"DH", "DK", "HF", "SEg", "WB", "Dg", "BGn", "RGn", "GGn", "FEg", "LHk", "VGn", "FiD",
	"CP", "FO", "FS", "FBo", "HT", "MGn", "+P", "W", "WH", 
	/* promoted and unpromoted, yep FO double is unnecessary */
	 "+Ph", "+C", "+N", "FO", "+I", "+G", "+Ky", "+FL",
	"+B", "+R", "+DH", "+DK", "+SSd", "+HF", "+SEg", "+VSd",
	"+FK", "+Ln", "+WB", "+BGn", "+RGn"
};


unsigned char *TeX_light_piece_string[PIECE_TYPES] = {
	"Fuhyo", "Oosho", "Suizo", "Kinsho", "Ginsho", "Dosho", "Tessho", "Mohyo", "Keima", "Kyosha", "Hansha", "Shahei", "Moko",
	"Kirin", "Hoo", "Hono", "Ogyo", "Kohei", "Kengyo", "Kenhei", "Hisha", "Shishi", "Kakugyo",
	"Ryume", "Ryuo", "Kakuo", "Hiju", "Suigyu", "Inu", "Kakusho", "Hisho", "Taisho", "Honju", "Shiyo", "Fukusho", "Kaki",
	"Red{Taishi}", "Red{Higyu}", "Red{Hiroku}", "Red{Honcho}", "Red{Shiteno}", "Red{Suisho}", "Red{Kinsho}", "Red{Keigei}", "Red{Hakku}", 
	/* promoted and unpromoted */
	 "Red{Hono}", "Red{Ogyo}", "Red{Ohei}", "Red{Shugyo}", "Red{Shuhei}", "Red{Hisha}", "Red{Shishi}", "Red{Kakugyo}",
	"Red{Ryume}", "Red{Ryuo}", "Red{Kakuo}", "Red{Hiju}", "Red{Suigyu}", "Red{Kakusho}", "Red{Hisho}", "Red{Shahei}",
 "Red{Honju}", "Red{Shiyo}", "Red{Kaki}", "Red{Fukusho}", "Red{Taisho}"
};

unsigned char *TeX_dark_piece_string[PIECE_TYPES] = {
	"FUHYO", "GYOKUSHO", "SUIZO", "KINSHO", "GINSHO", "DOSHO", "TESSHO", "MOHYO", "KEIMA", "KYOSHA", "HANSHA", "SHAHEI", "MOKO",
	"KIRIN", "HOO", "HONO", "OGYO", "KOHEI", "KENGYO", "KENHEI", "HISHA", "SHISHI", "KAKUGYO",
	"RYUME", "RYUO", "KAKUO", "HIJU", "SUIGYU", "INU", "KAKUSHO", "HISHO", "TAISHO", "HONJU", "SHIYO", "FUKUSHO", "KAKI",
	"Red{TAISHI}", "Red{HIGYU}", "Red{HIROKU}", "Red{HONCHO}", "Red{SHITENO}", "Red{SUISHO}", "Red{KINSHO}", "Red{KEIGEI}", "Red{HAKKU}", 
	/* promoted and unpromoted */
	 "Red{HONO}", "Red{OGYO}", "Red{OHEI}", "Red{SHUGYO}", "Red{SHUHEI}", "Red{HISHA}", "Red{SHISHI}", "Red{KAKUGYO}",
	"Red{RYUME}", "Red{RYUO}", "Red{KAKUO}", "Red{HIJU}", "Red{SUIGYU}", "Red{KAKUSHO}", "Red{HISHO}", "Red{SHAHEI}",
 "Red{HONJU}", "Red{SHIYO}", "Red{KAKI}", "Red{FUKUSHO}", "Red{TAISHO}"
};

unsigned char *kanji_piece_string[PIECE_TYPES] = {
	"歩 ", "玉 ", "象 ", "金 ", "銀 ", "銅 ", "鉄 ", "猛 ", "桂 ", "香 ", "反 ", "車 ", "虎 ",
	"麒 ", "鳳 ", "奔 ", "横 ", "横!", "竪 ", "竪!", "飛 ", "獅 ", "角 ",
	"馬 ", "龍 ", "鷹 ", "鷲 ", "水 ", "犬 ", "角!", "飛!", "大!", "奔!", "獅!", "副!", "火 ",
	"\e[31m子\e[30m ", "\e[31m牛\e[30m ", "\e[31m鹿\e[30m ", "\e[31m猪\e[30m ", "\e[31m天\e[0m ", "\e[31m雜\e[30m ", "\e[31m金\e[30m ", "\e[31m鯨\e[30m ", "\e[31m駒\e[30m ", 
	/* promoted and unpromoted */
	 "\e[31m奔\e[30m ", "\e[31m横\e[30m ", "\e[31m横!\e[30m", "\e[31m竪\e[30m ", "\e[31m竪!\e[30m", "\e[31m飛\e[30m ", "\e[31m獅\e[30m ", "\e[31m角\e[30m ",
	"\e[31m馬\e[30m ", "\e[31m龍\e[30m ", "\e[31m鷹\e[30m ", "\e[31m鷲\e[30m ", "\e[31m水\e[30m ", "\e[31m角!\e[30m", "\e[31m飛!\e[30m", "\e[31m車\e[30m ",
 "\e[31m奔!\e[30m", "\e[31m獅!\e[0m", "\e[31m火\e[30m ", "\e[31m副!\e[30m", "\e[31m大!\e[30m",
};

unsigned char *upper_kanji_string[PIECE_TYPES] = {
	"歩", "玉", "酔", "金", "銀", "銅", "鉄", "猛", "桂", "香", "反", "車", "盲",
	"麒", "鳳", "奔", "横", "横", "竪", "竪", "飛", "獅", "角",
	"龍", "龍", "角", "飛", "水", "犬", "角", "飛", "大", "奔", "獅", "副", "火",
	"\e[31m太\e[30m", "\e[31m飛\e[30m", "\e[31m飛\e[30m", "\e[31m奔\e[30m", "\e[31m四\e[0m", "\e[31m雜\e[30m", "\e[31m金\e[30m", "\e[31m鯨\e[30m", "\e[31m白\e[30m", 
	/* promoted and unpromoted */
	 "\e[31m奔\e[30m", "\e[31m横\e[30m", "\e[31m横\e[30m", "\e[31m竪\e[30m", "\e[31m竪\e[30m", "\e[31m飛\e[30m ", "\e[31m獅\e[30m", "\e[31m角\e[30m",
	"\e[31m龍\e[30m", "\e[31m龍\e[30m", "\e[31m角\e[30m", "\e[31m飛\e[30m", "\e[31m水\e[30m", "\e[31m角!\e[30m", "\e[31m飛!\e[30m", "\e[31m車\e[30m",
 "\e[31m奔\e[30m", "\e[31m獅\e[0m", "\e[31m火\e[30m", "\e[31m副\e[30m", "\e[31m大\e[30m"
};

unsigned char *lower_kanji_string[PIECE_TYPES] = {
	"兵", "将", "象", "将", "将", "将", "将", "豹", "馬", "車", "車", "兵", "虎",
	"麟", "凰", "王", "行", "兵", "行", "兵", "車", "子", "行",
	"馬", "王", "鷹", "鷲", "牛", "  ", "将", "将", "将", "鷲", "鷹", "将", "鬼",
	"\e[31m子\e[30m", "\e[31m牛\e[30m", "\e[31m鹿\e[30m", "\e[31m猪\e[30m", "\e[31m天\e[0m", "\e[31m将\e[30m", "\e[31m将\e[30m", "\e[31m鯢\e[30m", "\e[31m駒\e[30m", 
	/* promoted and unpromoted */
	 "\e[31m王\e[30m", "\e[31m行\e[30m", "\e[31m兵\e[30m", "\e[31m行\e[30m", "\e[31m兵\e[30m", "\e[31m車\e[30m ", "\e[31m子\e[30m", "\e[31m行\e[30m",
	" \e[31m馬\e[30m", "\e[31m王\e[30m", "\e[31m鷹\e[30m", "\e[31m鷲\e[30m", "\e[31m牛\e[30m", "\e[31m将\e[30m", "\e[31m将\e[30m", "\e[31m兵\e[30m",
 "\e[31m王\e[30m", "\e[31m鷹\e[0m", "\e[31m鬼\e[30m", "\e[31m将\e[30m", "\e[31m将\e[30m"
};

char *color_string[6] = {
  "\e[49m", /* NO_MOVE (default) */
  "\e[42m",/* NORMAL (green) */
  "\e[46m", /* HYPOTHETICAL (cyan) */
  "\e[45m", /* DANGER (purple) */
  "\e[41m", /* BURN (red) */
  "\e[40m" /*ALREADY_THERE (black)*/

};

char *inf_color[5] = {
  "\e[49m", /* NO_MOVE (default) */
  "\e[42m",/* NORMAL (green) */
  "\e[46m", /* HYPOTHETICAL or jump (cyan) */
  "\e[41m", /* BURN (red) */
  "\e[40m" /* shouldn't happen (black) */
};

/* the initial board state */

int init_boardcolor[NUMSQUARES] = {
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	1, 66, 1, 1, 66, 1, 1, 1, 1, 1, 1, 66, 1, 1, 66, 1,
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 1,	1, 1, 1, 1, 1, 1, 1, 1,
	66, 66, 66, 66, 1, 66, 66, 66, 66, 66, 66, 1, 66, 66, 66, 66,
	66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66,
	66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66,
	66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66,
	66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66,
	66, 66, 66, 66, 0, 66, 66, 66, 66, 66, 66, 0, 66, 66, 66, 66,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 66, 0, 0, 66, 0, 0, 0, 0, 0, 0, 66, 0, 0, 66, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
};

int init_piece[NUMSQUARES] = {
	9, 8, 7, 6, 5, 4, 3, 2, 1, 3, 4, 5, 6, 7, 8, 9,
	10, 66, 11, 11, 66, 12, 14, 15, 21, 13, 12, 66, 11, 11, 66, 10,
	17, 19, 22, 23, 24, 27, 35, 32, 33, 35, 27, 24, 23, 22, 19, 17,
	16, 18, 20, 25, 26, 29, 30, 34, 31, 30, 29, 26, 25, 20, 18, 16,
	0, 0, 0, 0, 0, 0, 0, 0,	0, 0, 0, 0, 0, 0, 0, 0,
	66, 66, 66, 66, 28, 66, 66, 66, 66, 66, 66, 28, 66, 66, 66, 66,
	66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66,
	66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66,
	66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66,
	66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66,
	66, 66, 66, 66, 28, 66, 66, 66, 66, 66, 66, 28, 66, 66, 66, 66,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	16, 18, 20, 25, 26, 29, 30, 31, 34, 30, 29, 26, 25, 20, 18, 16,
	17, 19, 22, 23, 24, 27, 35, 33, 32, 35, 27, 24, 23, 22, 19, 17,
	10, 66, 11, 11, 66, 12, 13, 21, 15, 14, 12, 66, 11, 11, 66, 10,
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
  100, 100, 100,
  100, 100, 100,
  100, 100,
  100, 100, 100,
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
  "Bishop General (+HF)", "Rook General (+SEg)", "Chariot Soldier (+VSd)", 
/* pfree_eagle, plion_hawk, pfire_demon, pvice_general, pgreat_general */
  "Free Eagle (+FK)", "Lion Hawk (+Ln)", "Fire Demon (+WB)",
  "Vice General (+BGn)", "Great General (+RGn)",
  "(empty) "
};

#endif
