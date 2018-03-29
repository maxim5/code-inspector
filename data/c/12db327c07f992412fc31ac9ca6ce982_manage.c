/*
 * manage.c
 * Author: Steve Dunn, MIT Wearable Computing Group
 * copyright (C) 2001 MIT Media Lab
 *
 * Manage the window list
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>

#include "anduin.h"

static AppWin *winList = NULL; // list of known client windows
AppWin *mainAppList = NULL; // list of main apps
AppWin *appletList = NULL; // list of applets

static int nextWinNum = 0;

void sendProtocolUpdate(AppWin *appWin, char *protoType);

//---------------------------------------------------------

void dumpApps(AppWin *list) {
	AppWin *ptr = list;

	printf("Apps:\n");
	while(ptr != NULL) {
		printf("App: %s window: %ld isMapped: %d\n", ptr->name, ptr->win, ptr->isMapped);
		ptr = ptr->next;
	}
	putchar('\n');
}

//---------------------------------------------------------

void removeFromAppList(AppWin **list, AppWin *appWin) {
	AppWin *ptr = *list;

	if(appWin == NULL || *list == NULL)
		return;

	if(*list == appWin) {
		*list = ptr->next;
	}
	else {
		while(ptr->next != NULL) {
			if(ptr->next == appWin) {
				ptr->next = ptr->next->next;
				break;
			}

			ptr = ptr->next;
		}
	}
	appWin->next = NULL;
}

//---------------------------------------------------------

void addToAppList(AppWin **list, AppWin *appWin) {
	if(appWin == NULL)
		return;

	removeFromAppList(list, appWin);
	appWin->next = *list;

	*list = appWin;
}

// Shift ahead to the next app in the line
void shiftNext() {
	AppWin *ptr, *p;

	if (mainAppList == NULL || mainAppList->next == NULL)
		return;

	ptr = mainAppList;
	removeFromAppList(&mainAppList, ptr);

	for (p = mainAppList; p->next != NULL;)  {
		p = p->next;
	}

	p->next = ptr;
	if(! mainAppList->isMapped) {
		XMapWindow(display, mainAppList->win);
		XMapWindow(display, mainAppList->parent);
	}
	XRaiseWindow(display, mainAppList->parent);

	showAppList();
	reFocus();
}

// Make the last app the current app, i.e. go from:
// app1 app2 app3 app4
// to:
// app4 app1 app2 app3
void shiftPrev() {
	AppWin *p;

	if (mainAppList == NULL || mainAppList->next == NULL)
		return;

	for (p = mainAppList; p->next != NULL;) {
		p = p->next;
	}
	addToAppList(&mainAppList, p);
	if(! mainAppList->isMapped) {
		XMapWindow(display, mainAppList->win);
		XMapWindow(display, mainAppList->parent);
	}
	XRaiseWindow(display, mainAppList->parent);

	showAppList();
	reFocus();
}

//---------------------------------------------------------

void showAppList() {
	int i, len, maxLen, overrun, numOver, maxStrLen, diff;
	AppWin *ptr;
	char *dataStr;
	XColor active, inactive, exact;
	int screen = 0;
	int textWidth, textHeight, textPos;
	XFontStruct *fontInfo;
	GContext gContext;
	char *defaultStr = "Unknown";
	char *str;

	if(taskGC == NULL)
		return;

	if(mainAppList == NULL) {
		XClearWindow(display, taskWin);
		return;
	}

	dataStr = malloc(rMAX_TASK_LENGTH+1);

	XAllocNamedColor(display, DefaultColormap(display, screen), rACTIVE_TASK_COLOR, &active, &exact);
	XAllocNamedColor(display, DefaultColormap(display, screen), rINACTIVE_TASK_COLOR, &inactive, &exact);

	gContext = XGContextFromGC(taskGC);
	fontInfo = XQueryFont(display, gContext);
	textHeight = fontInfo->ascent+fontInfo->descent;

	dataStr[rMAX_TASK_LENGTH] = '\0';

	XClearWindow(display, taskWin);

	// printf("Start max width: %d\n", fontInfo->max_bounds.width);
	maxLen = rMAX_TASK_LENGTH;
	overrun = 1;
	while(overrun > 0 && maxLen > 1) {
		numOver = 0;
		maxStrLen = 0;
		textPos = rTASK_TEXT_OFFSET;

		for(ptr=mainAppList, i=0; ptr != NULL && i<rMAX_TASKS_LISTED; ptr=ptr->next) {
			// if(! ptr->isMapped)
				// continue;
			i++;

			if(ptr->name == NULL) 
				str = defaultStr;
			else 
				str = ptr->name;

			len = strlen(str);
			if(len > maxLen) {
				len = maxLen;
				numOver++;
			}
			if(len > maxStrLen)
				maxStrLen = len;
			textWidth = XTextWidth(fontInfo, str, len);
			textPos += textWidth+rTASK_TEXT_SPACING+rTASK_TEXT_PADDING;
		}

		overrun = textPos - (taskWidth - 2*rTASK_TEXT_OFFSET);
		if(overrun > 0) {
			// printf("text len: %d max width: %d\n", textPos, taskWidth - 2*rTASK_TEXT_OFFSET);
			if(maxStrLen < maxLen)
				maxLen = maxStrLen;
			if(numOver == 0)
				numOver = i;
			if(fontInfo->max_bounds.width > 0)
				diff = (overrun/fontInfo->max_bounds.width) / i + 1;
			else
				diff = 1;
			// printf("overrun: %d num entries: %d numOver: %d maxLen: %d diff: %d\n",overrun,i,numOver,maxLen,diff);
			maxLen -= diff;
		}
	}
	if(maxLen < 1)
		maxLen = 1;
	// printf("overrun: %d numOver: %d maxLen: %d\n", overrun, numOver, maxLen);

	textPos = rTASK_TEXT_OFFSET;
	for(ptr=mainAppList, i=0; ptr != NULL && i<rMAX_TASKS_LISTED; i++, ptr=ptr->next) {
		if(ptr->name == NULL)
			strncpy(dataStr, defaultStr, maxLen);
		else
			strncpy(dataStr, ptr->name, maxLen);
		dataStr[maxLen] = '\0';

		textWidth = XTextWidth(fontInfo, dataStr, strlen(dataStr));

		if(i == 0)
			XSetForeground(display, taskGC, active.pixel);
		else
			XSetForeground(display, taskGC, inactive.pixel);

		XDrawString(display, taskWin, taskGC, 
			textPos, rTASK_TEXT_OFFSET+fontInfo->ascent, 
			dataStr, strlen(dataStr));
		XDrawRectangle(display, taskWin, taskGC, 
			textPos - rTASK_TEXT_PADDING, rTASK_TEXT_OFFSET-rTASK_TEXT_PADDING,
			textWidth+2*rTASK_TEXT_PADDING, textHeight+2*rTASK_TEXT_PADDING);
		textPos += textWidth+rTASK_TEXT_SPACING+rTASK_TEXT_PADDING;
	}

	XFree(fontInfo);
	free(dataStr);
}

//---------------------------------------------------------

void reorderApplets() {
	AppWin *top, *ptr, *prev;
	AppWin *app1, *app2;
	int i;

	top = appletList;
	while(top != NULL) {
		ptr = top;
		prev = NULL;

		// printf("  %s - %d\n", ptr->name, ptr->priority);
		while(ptr != NULL) {
			if(ptr->next != NULL && ptr->priority < ptr->next->priority) {
				// printf("Flip\n");
				app1 = ptr;
				app2 = ptr->next;

				app1->next = app2->next;
				app2->next = app1;
				if(prev == NULL) {
					if(top == appletList) {
						top = app2;
						appletList = top;
					}
					else {
						top = app2;
					}
				}
				else {
					prev->next = app2;
				}

				ptr = app1;
				prev = app2;
			}
			else {
				prev = ptr;
				ptr = ptr->next;
			}
		}

		top = top->next;
	}

	ptr = appletList;
	i = 0;
	while(ptr != NULL) {
		setAppletPos(ptr, i);

		ptr = ptr->next;
		i++;
	}
}

//---------------------------------------------------------

void reFocus() {
	int screen = 0;

	if(mainAppList == NULL)
		return;

	if(mainAppList->next != NULL && mainAppList->next->isMapped) {
		XSetWindowBackground(display, mainAppList->next->parent, 
			screens[screen].inactive);
		XClearWindow(display, mainAppList->next->parent);
		drawTitle(mainAppList->next);
	} 

	if(mainAppList->isMapped) {
		XSetWindowBackground(display, mainAppList->parent, 
			screens[screen].active);
		XClearWindow(display, mainAppList->parent);
		drawTitle(mainAppList);

		if(rVERBOSE) tickerMsg("Set Focus");
		XSetInputFocus(display, mainAppList->win, RevertToPointerRoot, CurrentTime);
	}
	else {
		fprintf(stderr, "Active Win %s is unmapped\n", mainAppList->name); // sdunn
	}
}

//---------------------------------------------------------

void setActiveWin(AppWin *appWin) {
	if(appWin == NULL)
		return;
	
	if(appWin == mainAppList) {
		reFocus();
		return;
	}
	
	appWin->timestamp = time(NULL);

	// tickerMsg("setting Active Win");
	addToAppList(&mainAppList, appWin);

	if(! appWin->isMapped) {
		XMapWindow(display, appWin->win);
		XMapWindow(display, appWin->parent);
	}
	XRaiseWindow(display, appWin->parent);

	showAppList();
	reFocus();
}

//---------------------------------------------------------

void nextWin() {
	if(mainAppList == NULL)
		return;
	setActiveWin(mainAppList->next);
}

//---------------------------------------------------------

AppWin* getWin(Window win) {
	AppWin *ptr = winList;

	while(ptr != NULL) {
		// if(ptr->win == win || (ptr->mode == MAIN_APP && ptr->parent == win))
		if(ptr->win == win)
			return ptr;
		ptr = ptr->nextApp;
	}

	return NULL;
}

//---------------------------------------------------------

AppWin* getWinFromParent(Window win) {
	AppWin *ptr = winList;

	while(ptr != NULL) {
		if(ptr->parent != None && ptr->parent == win)
			return ptr;
		ptr = ptr->nextApp;
	}

	return NULL;
}

//---------------------------------------------------------

AppWin* addWin(Window win) {
	AppWin *appWin;
	XWindowAttributes attr;
	XTextProperty name;
	int maxDefaultName = 15;
	Window prop = None;

	if(! XGetWindowAttributes(display, win, &attr))
		return NULL;

	if(rIGNORE_TRANSIENTS && XGetTransientForHint(display, win, &prop)) {
		if(rVERBOSE) tickerMsg("Found transient Window");
		return NULL;
	}

	if(rVERBOSE) printf("  window - x: %d y: %d width: %d height: %d\n",
		attr.x, attr.y, attr.width, attr.height);

	appWin = (AppWin*) malloc(sizeof(AppWin));
	appWin->win = win;
	appWin->parent = None;
	appWin->x = attr.x;
	appWin->y = attr.y;
	appWin->width = attr.width;
	appWin->height = attr.height;
	appWin->actualWidth = 100; // sdunn
	appWin->actualHeight = 100; // sdunn
	appWin->root = attr.root;
	appWin->screen = getScreen(appWin->root);

	if(attr.override_redirect)
		appWin->mode = OVERRIDE;
	else
		appWin->mode = UNASSIGNED;

	XSelectInput(display, appWin->win, PropertyChangeMask);

	appWin->focusNotify = 0;
	appWin->deleteNotify = 0;
	appWin->saveNotify = 0;
	getWinPriority(appWin);
	getWinTransientTag(appWin);

	if(attr.class == InputOnly)
		appWin->inputOnly = 1;
	else
		appWin->inputOnly = 0;

	appWin->timestamp = time(NULL);

	appWin->isSaved = 0;
	if(attr.map_state == IsViewable)
		appWin->isMapped = 1;
	else
		appWin->isMapped = 0;

	if(XGetWMName(display, appWin->win, &name) != 0) {
		appWin->name = (char*) malloc(strlen(name.value)+1);
		strcpy(appWin->name, name.value);
	}
	else {
		appWin->name = (char*) malloc(maxDefaultName+1);
		snprintf(appWin->name, maxDefaultName, "Window %d", nextWinNum++);
		appWin->name[maxDefaultName] = '\0';
	}

	appWin->next = NULL;

	appWin->nextApp = winList;
	winList = appWin;

	getWinProtocols(appWin);
	getWinPriority(appWin);
	getWinAppletTag(appWin);

	return appWin;
}

//---------------------------------------------------------

void removeWin(AppWin *appWin, int destroyClient) {
	XWindowChanges xwc;
	AppWin *ptr;
	unsigned int mask;

	if(appWin == NULL)
		return;

	// printf("remove win %ld\n", appWin->win);
	// if(appWin->mode == MAIN_APP) printf("remove parent %ld\n", appWin->parent);

	if(appWin->mode == MAIN_APP || appWin->mode == APPLET) {
		if(destroyClient || shutdownStarted)
			unassignWin(appWin, True);
		else
			unassignWin(appWin, False);
	}

	if(winList == appWin) {
		winList = appWin->nextApp;
	}
	else {
		ptr = winList;
		while(ptr->nextApp != NULL) {
			if(ptr->nextApp == appWin) {
				ptr->nextApp = ptr->nextApp->nextApp;
				break;
			}

			ptr = ptr->nextApp;
		}
	}
	appWin->nextApp = NULL;

	if(! shutdownStarted) {
		if(appWin == mainAppList)
			nextWin();
		reorderApplets();
	}

	if(destroyClient) {
		XReparentWindow(display, appWin->win, sandboxWin, 0, 0);
		if (appWin->deleteNotify) {
			// tickerMsg("Soft Kill");
			sendProtocolUpdate(appWin, "WM_DELETE_WINDOW");
		}
		else {
			// tickerMsg("Hard Kill");
			XKillClient(display, appWin->win);
		}
	}
	else if(shutdownStarted) {
		xwc.x = appWin->x;
		xwc.y = appWin->y;
		xwc.width = appWin->width;
		xwc.height = appWin->height;
		xwc.border_width = 1;
		mask = CWX | CWY | CWWidth | CWHeight;
		if(! appWin->inputOnly) 
			mask |= CWBorderWidth;
		XConfigureWindow(display, appWin->win, mask, &xwc); 
		XReparentWindow(display, appWin->win, appWin->root, appWin->x, appWin->y);

		if(appWin->isMapped)
			XMapWindow(display, appWin->win);
		else
			XUnmapWindow(display, appWin->win);
	}

	free(appWin);
	showAppList();
}

//---------------------------------------------------------

int isRoot(Window win) {
	int i;

	for(i=0; i<numScreens; i++) {
		if(win == screens[i].root)
			return 1;
	}
	return 0;
}

//---------------------------------------------------------

int isParent(Window win) {
	AppWin *ptr;

	ptr = mainAppList;
	while(ptr != NULL) {
		if(win == ptr->parent)
			return 1;
		ptr = ptr->nextApp;
	}
	return 0;
}

//---------------------------------------------------------

void removeAll() {
	AppWin *tmp = NULL;

	while(winList != NULL && tmp != winList) {
		tmp = winList;
		removeWin(winList, False);
	}
}

//---------------------------------------------------------

void sendProtocolUpdate(AppWin *appWin, char *protoType) {
	Atom protocolsAtom = XInternAtom(display, "WM_PROTOCOLS", False);
	Atom deleteAtom = XInternAtom(display, protoType, False);
	XEvent event;

	memset(&event, 0, sizeof(XEvent));
	event.xclient.type = ClientMessage;
	event.xclient.message_type = protocolsAtom;
	event.xclient.window = appWin->win;
	event.xclient.format = 32;
	event.xclient.data.l[0] = deleteAtom;
	event.xclient.data.l[1] = CurrentTime;

	XSendEvent(display, appWin->win, False, 0, &event);
}

//---------------------------------------------------------

int inList(Window win, Window *list, int numWins) {
	int i;

	for(i=0; i<numWins; i++) {
		if(list[i] == win)
			return 1;
	}

	return 0;
}

//---------------------------------------------------------

void printDebugTree(Window myRoot, int level) {
	char real_buffer[256];
	char *buffer;
	int i = 0;
	int numWins;
	Window winRoot, winParent;
	Window *winChildren;
	int screen = 0;
	AppWin *appWin;

	if(level > 128) {
		tickerMsg("Error - too many window levels");
		return;
	}

	for(i=0; i<level; i++)
		real_buffer[i] = ' ';
	buffer = real_buffer+level;

	XQueryTree(display, myRoot, &winRoot, &winParent, &winChildren, &numWins);
	for(i=0; i<numWins; i++) {
		if(winChildren[i] == taskWin) 
			sprintf(buffer, "%d-%d Win: %ld - taskWin", level, i, winChildren[i]);
		else if(winChildren[i] == appletWin) 
			sprintf(buffer, "%d-%d Win: %ld - appletWin", level, i, winChildren[i]);
		else if(winChildren[i] == tickerWin) 
			sprintf(buffer, "%d-%d Win: %ld - tickerWin", level, i, winChildren[i]);
		else if(winChildren[i] == sandboxWin) 
			sprintf(buffer, "%d-%d Win: %ld - sandboxWin", level, i, winChildren[i]);
		else if(winChildren[i] == screens[screen].root) 
			sprintf(buffer, "%d-%d Win: %ld - root", level, i, winChildren[i]);
		else {
			appWin = getWin(winChildren[i]);
			if(appWin != NULL) {
				if(appWin->mode == UNASSIGNED )
					sprintf(buffer, "%d-%d Win: %ld - AppWin - Unassigned - %s", level, i, winChildren[i], appWin->name);
				else if(appWin->mode == MAIN_APP )
					sprintf(buffer, "%d-%d Win: %ld - AppWin - MainApp - %s", level, i, winChildren[i], appWin->name);
				else if(appWin->mode == APPLET )
					sprintf(buffer, "%d-%d Win: %ld - AppWin - Applet - %s", level, i, winChildren[i], appWin->name);
				else if(appWin->mode == OVERRIDE )
					sprintf(buffer, "%d-%d Win: %ld - AppWin - Override - %s", level, i, winChildren[i], appWin->name);
				else
					sprintf(buffer, "%d-%d Win: %ld - AppWin -  - %s", level, i, winChildren[i], appWin->name);
			}
			else {
				appWin = getWinFromParent(winChildren[i]);
				if(appWin != NULL) {
					sprintf(buffer, "%d-%d Win: %ld - Main App Parent - %s", level, i, winChildren[i], appWin->name);
				}
				else {
					sprintf(buffer, "%d-%d Win: %ld -", level, i, winChildren[i]);
				}
			}
		}

		tickerMsg(real_buffer);

		// sprintf(buffer, "Level: %d-%d Num Wins: %d", level, i, numWins);
		// tickerMsg(buffer);

		printDebugTree(winChildren[i], level+1);
	}
}

//---------------------------------------------------------

void printDebug() {
	char buffer[128];
	char status[64];
	char pWin[64];
	int i = 0;
	int numWins;
	Window myRoot;
	Window winRoot, winParent;
	Window *winChildren;
	int screen = 0;
	AppWin *appWin;
	XWindowAttributes attr;

	if(rVERBOSE) tickerMsg("exposeDebug");

	sprintf(buffer, "Num screens: %d", ScreenCount(display) );
	tickerMsg(buffer);

	myRoot = screens[screen].root;
	XQueryTree(display, myRoot, &winRoot, &winParent, &winChildren, &numWins);

	sprintf(buffer, "Specified Root: %ld Root: %ld Parent: %ld Num Wins: %d", myRoot, winRoot, winParent, numWins);
	tickerMsg(buffer);

	if(rVERBOSE) printf("%d windows in screen %d\n", numWins, screen);
	printDebugTree(myRoot, 0);

	i = 0;

	appWin = winList;
	while(appWin != NULL) {
		if(XGetWindowAttributes(display, appWin->win, &attr))
			sprintf(status, "ALIVE");
		else
			sprintf(status, "DEAD ");

		if( inList(appWin->win, winChildren, numWins) )
			strcpy(pWin, "- win Root");
		else
			*pWin = '\0';

		if(appWin->mode == UNASSIGNED )
			sprintf(buffer, "%s AppWin Unassigned %s - Win: %ld : %s", status, pWin, appWin->win, appWin->name);
		else if(appWin->mode == MAIN_APP ) {
			if( inList(appWin->parent, winChildren, numWins) )
				strcpy(pWin, "- parent Root");
			else
				*pWin = '\0';

			sprintf(buffer, "%s AppWin MainApp %s - Win: %ld : %s", status, pWin, appWin->win, appWin->name);
		}
		else if(appWin->mode == APPLET ) {
			if( appWin->parent == appletWin )
				strcpy(pWin, "- parent AppletWin");

			sprintf(buffer, "%s AppWin Applet %s - Win: %ld : %s", status, pWin, appWin->win, appWin->name);
		}
		else if(appWin->mode == OVERRIDE )
			sprintf(buffer, "%s AppWin Override %s - Win: %ld : %s", status, pWin, appWin->win, appWin->name);
		else
			sprintf(buffer, "%s AppWin Unknown %s - Win: %ld : %s", status, pWin, appWin->win, appWin->name);

		tickerMsg(buffer);

		appWin = appWin->nextApp;
		i++;
	}
}

