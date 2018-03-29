/*-------------------------------------------------------*/
/* manage.c	( NTHU CS MapleBBS Ver 3.10 )		 */
/*-------------------------------------------------------*/
/* target : 看板管理				 	 */
/* create : 95/03/29				 	 */
/* update : 96/04/05				 	 */
/*-------------------------------------------------------*/


#include "bbs.h"


extern BCACHE *bshm;
extern char *battr_tbl[];

extern XZ xz[];


#ifdef HAVE_TERMINATOR
/* ----------------------------------------------------- */
/* 站長功能 : 拂楓落葉斬				 */
/* ----------------------------------------------------- */


extern char xo_pool[];


#define MSG_TERMINATOR	"《拂楓落葉斬》"

int
post_terminator(xo)		/* Thor.980521: 終極文章刪除大法 */
    XO *xo;
{
    int mode, type;
    HDR *hdr;
    char keyOwner[80], keyTitle[TTLEN + 1], buf[80];

    if (!HAS_PERM(PERM_ALLBOARD))
	return XO_FOOT;

    mode = vans(MSG_TERMINATOR "刪除 (1)本文作者 (2)本文標題 (3)自定？[Q] ") - '0';

    if (mode == 1)
    {
	hdr = (HDR *) xo_pool + (xo->pos - xo->top);
	strcpy(keyOwner, hdr->owner);
    }
    else if (mode == 2)
    {
	hdr = (HDR *) xo_pool + (xo->pos - xo->top);
	strcpy(keyTitle, str_ttl(hdr->title));		/* 拿掉 Re: */
    }
    else if (mode == 3)
    {
	if (!vget(b_lines, 0, "作者：", keyOwner, 73, DOECHO))
	    mode ^= 1;
	if (!vget(b_lines, 0, "標題：", keyTitle, TTLEN + 1, DOECHO))
	    mode ^= 2;
    }
    else
    {
	return XO_FOOT;
    }

    type = vans(MSG_TERMINATOR "刪除 (1)轉信板 (2)非轉信板 (3)所有看板？[Q] ");
    if (type < '1' || type > '3')
	return XO_FOOT;

    sprintf(buf, "刪除%s：%.35s 於%s板，確定嗎(Y/N)？[N] ", 
	    mode == 1 ? "作者" : mode == 2 ? "標題" : "條件", 
	    mode == 1 ? keyOwner : mode == 2 ? keyTitle : "自定", 
	    type == '1' ? "轉信" : type == '2' ? "非轉信" : "所有看");

    if (vans(buf) == 'y')
    {
	BRD *bhdr, *head, *tail;
	char tmpboard[BNLEN + 1];

	/* Thor.980616: 記下 currboard，以便復原 */
	strcpy(tmpboard, currboard);

	head = bhdr = bshm->bcache;
	tail = bhdr + bshm->number;
	do				/* 至少有 note 一板 */
	{
	    int fdr, fsize, xmode;
	    FILE *fpw;
	    char fpath[64], fnew[64], fold[64];
	    HDR *hdr;

	    xmode = head->battr;
	    if ((type == '1' && (xmode & BRD_NOTRAN)) || (type == '2' && !(xmode & BRD_NOTRAN)))
		continue;

	    /* Thor.980616: 更改 currboard，以 cancel post */
	    strcpy(currboard, head->brdname);

	    sprintf(buf, MSG_TERMINATOR "看板：%s \033[5m...\033[m", currboard);
	    outz(buf);
	    refresh();

	    brd_fpath(fpath, currboard, fn_dir);

	    if ((fdr = open(fpath, O_RDONLY)) < 0)
		continue;

	    if (!(fpw = f_new(fpath, fnew)))
	    {
		close(fdr);
		continue;
	    }

	    fsize = 0;
	    mgets(-1);
	    while (hdr = mread(fdr, sizeof(HDR)))
	    {
		xmode = hdr->xmode;

		if ((xmode & POST_MARKED) || 
			((mode & 1) && strcmp(keyOwner, hdr->owner)) ||
			((mode & 2) && strcmp(keyTitle, str_ttl(hdr->title))))
		{
		    if ((fwrite(hdr, sizeof(HDR), 1, fpw) != 1))
		    {
			fclose(fpw);
			close(fdr);
			goto contWhileOuter;
		    }
		    fsize++;
		}
		else
		{
		    /* 砍文並連線砍信 */

		    cancel_post(hdr);
		    hdr_fpath(fold, fpath, hdr);
		    unlink(fold);
		}
	    }
	    close(fdr);
	    fclose(fpw);

	    sprintf(fold, "%s.o", fpath);
	    rename(fpath, fold);
	    if (fsize)
		rename(fnew, fpath);
	    else
		contWhileOuter:
		    unlink(fnew);

	    btime_update(brd_bno(currboard));
	} while (++head < tail);

	/* 還原 currboard */
	strcpy(currboard, tmpboard);
	return XO_LOAD;
    }

    return XO_FOOT;
}
#endif	/* HAVE_TERMINATOR */


/* ----------------------------------------------------- */
/* 板主功能 : 修改板名					 */
/* ----------------------------------------------------- */


static int
post_brdtitle(xo)
    XO *xo;
{
    BRD *oldbrd, newbrd;

    oldbrd = bshm->bcache + currbno;
    memcpy(&newbrd, oldbrd, sizeof(BRD));

    /* itoc.註解: 其實呼叫 brd_title(bno) 就可以了，沒差，蠻幹一下好了 :p */
    vget(b_lines, 0, "看板主題：", newbrd.title, BTLEN + 1, GCARRY);

    if (memcmp(&newbrd, oldbrd, sizeof(BRD)) && vans(msg_sure_ny) == 'y')
    {
	memcpy(oldbrd, &newbrd, sizeof(BRD));
	rec_put(FN_BRD, &newbrd, sizeof(BRD), currbno, NULL);
	brd_classchange("gem/@/@"CLASS_INIFILE, oldbrd->brdname, &newbrd);
    }

    return XO_HEAD;
}


/* ----------------------------------------------------- */
/* 板主功能 : 修改進板畫面				 */
/* ----------------------------------------------------- */


static int
post_memo_edit(xo)
    XO *xo;
{
    int mode;
    char fpath[64];

    mode = vans("進板畫面 (D)刪除 (E)修改 (Q)取消？[E] ");

    if (mode != 'q')
    {
	brd_fpath(fpath, currboard, fn_note);

	if (mode == 'd')
	{
	    unlink(fpath);
	}
	else
	{
	    if (vedit(fpath, 0))	/* Thor.981020: 注意被talk的問題 */
		vmsg(msg_cancel);
	}
    }
    return XO_HEAD;
}

/* ----------------------------------------------------- */
/* 板主功能 : 改精華區畫面                               */
/* ----------------------------------------------------- */


static int
post_gemmemo_edit(xo)
    XO *xo;
{
    int mode;
    char fpath[64];

    mode = vans("精華區歡迎畫面 (D)刪除 (E)修改 (Q)取消？[E] ");

    if (mode != 'q')
    {
	brd_fpath(fpath, currboard, fn_gemnote);

	if (mode == 'd')
	{
	    unlink(fpath);
	}
	else
	{
	    if (vedit(fpath, 0))      /* Thor.981020: 注意被talk的問題 */
		vmsg(msg_cancel);
	}
    }
    return XO_HEAD;
}

/* ----------------------------------------------------- */
/* 板主功能 : 修改看板說明                               */
/* ----------------------------------------------------- */


#ifdef HAVE_BRD_EXPLAIN
static int
post_explain_edit(xo)
    XO *xo;
{
    int mode;
    char fpath[64];

    mode = vans("看板說明 (D)刪除 (E)修改 (Q)取消？[E] ");

    if (mode != 'q')
    {
	brd_fpath(fpath, currboard, fn_explain);
	if (mode == 'd')
	    unlink(fpath);
	else
	{
	    if (vedit(fpath, 0)) /* Thor.981020: 注意被talk的問題 */
		vmsg(msg_cancel);

	    return XO_HEAD;
	}
    }

    return XO_FOOT;
}
#endif


/* ----------------------------------------------------- */
/* 板主功能 : 修改發文綱領                               */
/* ----------------------------------------------------- */


static int
post_postlaw_edit(xo)       /* 板主自定文章發表綱領 */
    XO *xo;
{
    int mode;
    char fpath[64];

    mode = vans("文章發表綱領 (D)刪除 (E)修改 (Q)取消？[E] ");

    if (mode != 'q')
    {
	brd_fpath(fpath, currboard, FN_POSTLAW);

	if (mode == 'd')
	{
	    unlink(fpath);
	    return XO_FOOT;
	}

	if (vedit(fpath, 0))      /* Thor.981020: 注意被talk的問題 */
	    vmsg(msg_cancel);
	return XO_HEAD;
    }
    return XO_FOOT;
}

/* ----------------------------------------------------- */
/* 板主功能 : ＲＳＳ訂閱功能                             */
/* ----------------------------------------------------- */

static int post_rss(XO *xo)
{
    XO *xt;
    char fpath[256];

    if (!(bbstate & STAT_BOARD) && !HAS_PERM(PERM_SYSOP))
	return XO_NONE;

    pmsg("RSS訂閱\器上線測試, 有任何問題請到XDBug板回報");
    brd_fpath(fpath, currboard, fn_rssconf); 
    xz[XZ_RSS - XO_ZONE].xo = xt = xo_new(fpath);
    strcpy(xt->dir, fpath);
    xover(XZ_RSS);        
    free(xt);

    return XO_INIT;
}

#ifdef POST_PREFIX
/* ----------------------------------------------------- */
/* 板主功能 : 修改發文類別                               */
/* ----------------------------------------------------- */


static int
post_prefix_edit(xo)
    XO *xo;
{
    int i;
    FILE *fp;
    char mybuf[40];
    char fpath[64], buf[20], prefix[NUM_PREFIX][20], *menu[NUM_PREFIX + 4];
    char *prefix_def[NUM_PREFIX] =   /* 預設的類別 */
    {
	"[公告]","[新聞]","[閒聊]","[文件]","[創作]",
	"[測試]","[其他]","[問題]","[八卦]","[分享]"
    };

    if (!(bbstate & STAT_BOARD))
	return XO_NONE;
    i = vans("類別 (D)刪除 (E)修改 (Q)取消？[E] ");

    if (i == 'q')
	return XO_FOOT;

    brd_fpath(fpath, currboard, FN_PREFIX);

    if (i == 'd')
    {
	unlink(fpath);
	return XO_FOOT;
    }

    i = 0;

    if (fp = fopen(fpath, "r"))
    {
	for (; i < NUM_PREFIX; i++)
	{
	    char *ptr;
	    if (fgets(buf, 14, fp) == NULL)
		break;

	    ptr = strchr(buf, '\n');
	    if (ptr)
		*ptr = '\0';
	    sprintf(prefix[i], "%d.%s", i, buf);
	}
	fclose(fp);
    }
    else
    {
	/* 填滿至 NUM_PREFIX 個 */
	for (; i < NUM_PREFIX; i++)
	    sprintf(prefix[i], "%d.%s", i, prefix_def[i]);
    }

    if (!i)
	return XO_FOOT;

    int j;
    menu[0] = "0Q";
    for (j = 1; j <= i; j++)
	menu[j] = prefix[j - 1];
    menu[j++] = "C.變更類別數量";
    menu[j++] = "Q.離開";
    menu[j++] = NULL;

    do
    {
	/* 在 popupmenu 裡面按 左鍵 離開 */
	j = pans(3, 20, "文章類別", menu) - '0';
	if (j >= 0 && j < i)
	{
	    strcpy(buf, prefix[j] + 2);
	    if (vget(b_lines, 0, "類別：", buf, 13, GCARRY))
		strcpy(prefix[j] + 2, buf);
	}
	else if (j == 'c' - '0')
	{
	    char buf[3];
	    sprintf(buf, "%d", i);
	    if (vget(b_lines, 0, "請輸入新的類別數量(1 - 10)：", buf, 3, GCARRY))
	    {
		int num = atoi(buf);
		if (num <= 0 || num >NUM_PREFIX)
		    continue;
		if (num > i)
		{
		    int k;
		    for (k = i ; k < num; k++)
		    {
			sprintf(prefix[k], "%d.%s", k, prefix_def[k]);
			menu[k+1] = prefix[k];
		    }
		}
		i = num;
		menu[i+1] = "C.變更類別數量";
		menu[i+2] = "Q.離開";
		menu[i+3] = NULL;
	    }
	}
    } while (j != 'q' - '0');

    if (vans(msg_sure_ny) != 'y')
    {
	pmsg("取消設定");
	return XO_HEAD;
    }

    if (fp = fopen(fpath, "w"))
    {
	for (j = 0; j < i; j++)
	{
	    char *ptr;
	    sprintf(mybuf, "%s", prefix[j] + 2);
	    ptr = strchr(mybuf, '\n');
	    if (ptr)
		*ptr = '\0';
	    fprintf(fp, "%s\n", mybuf);
	}
	fclose(fp);
    }

    return XO_FOOT;

}
#endif      /* POST_PREFIX */


/* ----------------------------------------------------- */
/* 板主功能 : 修改板主名單				 */
/* ----------------------------------------------------- */


static int
post_changeBM(void)
{
    BRD *brd = bshm->bcache + currbno;
    int ret = brd_changeBM(brd);
    rec_put(FN_BRD, brd, sizeof(BRD), currbno, NULL); // TODO: check is needed.
    sprintf(currBM, "板主：%s", *(brd->BM) <= ' '  ? brd->battr & BRD_SERVICE ?
	            str_operators : "徵求中" : brd->BM);
    return ret;
}


#ifdef HAVE_MODERATED_BOARD
/* ----------------------------------------------------- */
/* 板主功能 : 看板權限					 */
/* ----------------------------------------------------- */


static int
post_brdlevel(xo)
    XO *xo;
{
    BRD *oldbrd, newbrd;

    oldbrd = bshm->bcache + currbno;
    memcpy(&newbrd, oldbrd, sizeof(BRD));

    switch (vansf("1)公開看板 2)秘密看板 3)好友看板 %s？[Q] ",
		HAS_PERM(PERM_ALLBOARD) ? "4)封印看板" : ""))
    {
	case '1':				/* 公開看板 */
	    newbrd.bmode = BMODE_OPEN;
	    newbrd.postlevel = PERM_POST;
	    newbrd.battr &= ~(BRD_NOSTAT | BRD_NOVOTE);
	    break;

	case '2':				/* 秘密看板 */
	    newbrd.postlevel = 0;
	    newbrd.bmode = BMODE_HIDE;
	    newbrd.battr |= (BRD_NOSTAT | BRD_NOVOTE);
	    break;

	case '3':				/* 好友看板 */
	    newbrd.bmode = BMODE_PAL;
	    newbrd.postlevel = 0;
	    newbrd.battr |= (BRD_NOSTAT | BRD_NOVOTE);
	    break;

	case '4':				/* 封印板 */
	    if (!HAS_PERM(PERM_ALLBOARD))
		return XO_HEAD;
	    newbrd.bmode = BMODE_SEAL;
	    newbrd.battr |= (BRD_NOSTAT | BRD_NOVOTE);
	    break;

	default:
	    return XO_HEAD;
    }

    if (memcmp(&newbrd, oldbrd, sizeof(BRD)) && vans(msg_sure_ny) == 'y')
    {
	memcpy(oldbrd, &newbrd, sizeof(BRD));
	rec_put(FN_BRD, &newbrd, sizeof(BRD), currbno, NULL);
    }

    return XO_HEAD;
}
#endif	/* HAVE_MODERATED_BOARD */


#ifdef HAVE_MODERATED_BOARD
/* ----------------------------------------------------- */
/* 板友名單：moderated board				 */
/* ----------------------------------------------------- */


static void
bpal_cache(char * fpath, int type) /* hrs.080412 : type = 1 可見/type = 2 水桶 */
{
    BPAL *bpal;

    bpal = type - 1 ? bshm->pbad + currbno : bshm->pcache + currbno;
    bpal->pal_max = image_pal(fpath, bpal->pal_spool);
}




static int
XoBM(xo)
    XO *xo;
{
    XO *xt;
    char fpath[64];
    char op;

    outl(b_lines - 5, NULL);
    outl(b_lines - 4, "不管是 公開/好友/隱藏 板：");
    outl(b_lines - 3, "1) 在可見名單中的帳號，一律可進入本看板並有閱\讀及發文權限。");
    outl(b_lines - 2, "2) 在水桶名單中的帳號，一律不可發文，名單中的壞人無法看見本板。");
    outl(b_lines - 1, ANSI_COLOR(1;31)"請注意: 名單設定完畢後，要在離開本介面後才會套用。" 
	    ANSI_RESET);
    op = vans("你要設定 1) 可見名單 2) 水桶名單 Q) 離開 [Q] ");

    if (op != '1' && op != '2')
	return XO_HEAD;
    op -= '0';
    brd_fpath(fpath, currboard, op - 1 ? fn_bad : fn_pal);
    xz[XZ_PAL - XO_ZONE].xo = xt = xo_new(fpath);
    xt->key =  op - 1 ? PALTYPE_PBAD : PALTYPE_BPAL;
    xover(XZ_PAL);		/* Thor: 進xover前, pal_xo 一定要 ready */

    /* build userno image to speed up, maybe upgreade to shm */

    bpal_cache(fpath, op);

    free(xt);

    vmsg("名單更新完畢。");

    return XO_INIT;
}
#endif	/* HAVE_MODERATED_BOARD */

/* ----------------------------------------------------- */
/* 看板閱讀紀錄                                          */
/* ----------------------------------------------------- */

static int
post_usies(xo)
    XO *xo;
{
    char fpath[64];

    brd_fpath(fpath, currboard, "usies");
    if (more(fpath, FOOTER_POST) >= 0 &&
	    vans("請問是否刪除這些看板閱\讀記錄(Y/N)？[N] ") == 'y')
	unlink(fpath);

    return XO_HEAD;
}

/* ----------------------------------------------------- */
/* 板主購買文章上限(expire上限)                          */
/* ----------------------------------------------------- */

static int
m_expire()
{
    BRD *oldbrd, newbrd;
    char buf[80];
    int pay;
    uschar op;

    if (HAS_STATUS(STATUS_COINLOCK))
    {
	vmsg(msg_coinlock);
	return XEASY;
    }

    oldbrd = bshm->bcache + currbno;
    memcpy(&newbrd, oldbrd, sizeof(BRD));

    move(b_lines - 3, 0);
    clrtobot();
    prints("目前看板的文章上限為 %d 篇/下限 %d 篇/保存 %d 天\n"
	    , oldbrd->ex_maxposts, oldbrd->ex_minposts, oldbrd->ex_maxtime);
    prints("一個單位為 50 篇文章或 20 天 , 一個單位需 " FEE_EXPIRE " 元\n");
    prints("你現在身上有 %d 元。", cuser.money);

    op = vans("你要買的是一單位 1) 50 篇文章 2) 20 天 [Q]");
    if (!op)
	return XO_HEAD;

    if (!vget(b_lines, 0, "你要買幾個單位呢? ", buf, 4, DOECHO))
	return XO_HEAD;
    pay = atoi(buf);
    if (pay <= 0)
	return XO_HEAD;

    if (pay * atoi(FEE_EXPIRE) > cuser.money)
    {
	vmsg("餘額不足。");
	return XO_HEAD;
    }

    newbrd.ex_maxposts += op - '1' ? 0 : 50 * pay; 
    newbrd.ex_minposts += op - '1' ? 0 : 50 * pay;
    newbrd.ex_maxtime  += op - '1' ? 20 * pay : 0;

    cuser.money -= atoi(FEE_EXPIRE) * pay;
    if (memcmp(&newbrd, oldbrd, sizeof(BRD)) && vans(msg_sure_ny) == 'y')
    {
	memcpy(oldbrd, &newbrd, sizeof(BRD));
	rec_put(FN_BRD, &newbrd, sizeof(BRD), currbno, NULL);
	vmsgf("您買了 %d 個單位，餘額 %d 元", pay, cuser.money);
    }

    return XO_HEAD;
}

/* ----------------------------------------------------- */
/* 自定文章範本                                          */
/* ----------------------------------------------------- */

static int
m_samplepost(void)
{
    int mode;
    char fpath[64], prefix[NUM_PREFIX][20], *menu[NUM_PREFIX + 3], buf[30];
    register int i, j;
    FILE *fp;

    brd_fpath(fpath, currboard, FN_PREFIX);

    if (fp = fopen(fpath, "r"))
    {
	for (i = 0; i < NUM_PREFIX; i++)
	{
	    char *ptr;
	    if (fgets(buf, 14, fp) == NULL)
		break;
	    ptr = strchr(buf, '\n');
	    if (ptr)
		*ptr = '\0';
	    sprintf(prefix[i], "%d.%s", i, buf);
	}
	fclose(fp);
    }
    else
    {
	vmsg("請先設定好文章類別。");
	return XO_HEAD;
    }

    menu[0] = "0Q";
    for (j = 1; j <= i; j++)
	menu[j] = prefix[j - 1];
    menu[j++] = "Q.離開";
    menu[j++] = NULL;

    do
    {
	clear();
	j = pans(3, 20, "文章範本相對應的文章類別", menu) - '0';
	if (j >= 0 && j < i)
	{
	    sprintf(buf, FN_SAMPLE ".%d", j);
	    brd_fpath(fpath, currboard, buf);
	    mode = vans("文章範本 (D)刪除 (E)修改 (Q)取消？[E] ");

	    if (mode != 'q')
	    {

		if (mode == 'd')
		    unlink(fpath);
		else
		{
		    if (vedit(fpath, 0))
			vmsg(msg_cancel);
		    else
			vmsg("儲存設定。");
		}
	    }
	}
    } while (j != 'q' - '0');

    return XO_HEAD;
}


/* ----------------------------------------------------- */
/* 板主選單						 */
/* ----------------------------------------------------- */


int
post_manage(xo)
    XO *xo;
{

#ifdef POPUP_ANSWER
    char *menu[] = 
    {
	"BQ",
	"BTitle  修改看板標題",
	"WMemo   編輯進板畫面",
	"GemMemo 改精華區畫面",
#ifdef HAVE_BRD_EXPLAIN
	"TExplain編輯看板說明",
#endif
	"PostLaw 編輯發文綱領",
	"ZRSS    ＲＳＳ器設定",
#ifdef POST_PREFIX
	"RPrefix 編輯文章類別",
#endif
	"Expire  購買文章上限",
	"ASample 編輯文章範本",
	"Manager 增減副板主",
	"Usies   觀察看板閱\讀記錄",

#  ifdef HAVE_MODERATED_BOARD
	"Level   公開/好友/秘密",
	"OPal    設定可見/水桶名單",
#  endif
	NULL
    };
#else
    char *menu = "◎ 板主選單 (B)標題 (W)進板 (G)精華 (P)綱領 (R)類別 (E)上限 (A)範本 (M)副板 (U)記錄 (Z)RSS"
#ifdef HAVE_BRD_EXPLAIN
	" (T)說明"
#endif
#  ifdef HAVE_MODERATED_BOARD
	" (L)權限 (O)名單"
#  endif
	"？[Q] ";
#endif

    if (!(bbstate & STAT_BOARD))
	return XO_NONE;

    utmp_mode(M_MBOARD);

#ifdef POPUP_ANSWER
    switch (pans(3, 20, "板主選單", menu))
#else
	switch (vans(menu))
#endif
	{
	    case 'b':
		return post_brdtitle(xo);

	    case 'w':
		return post_memo_edit(xo);

	    case 'g':
		return post_gemmemo_edit(xo);

	    case 'p':
		return post_postlaw_edit(xo);

	    case 'z':
		return post_rss(xo);

#ifdef POST_PREFIX
	    case 'r':
		return post_prefix_edit(xo);
#endif
	    case 'e':
		return m_expire();

	    case 'a':
		return m_samplepost();

	    case 'm':
		return post_changeBM();

	    case 'u':
		return post_usies(xo);

#ifdef HAVE_BRD_EXPLAIN
	    case 't':
		return post_explain_edit(xo);
#endif

#ifdef HAVE_MODERATED_BOARD
	    case 'l':
		return post_brdlevel(xo);

	    case 'o':
		return XoBM(xo);
#endif
	}

    return XO_HEAD;
}
