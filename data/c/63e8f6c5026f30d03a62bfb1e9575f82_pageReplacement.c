/**
 * Implementation for page replacement algorithms.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/**
 * Utility function for initializing the frame array.
 * -1 indicates an unassigned frame.
 */
void initializeFrames(int *frames, int frameCount) {
    int i;
    for (i = 0; i < frameCount; i++) {
        frames[i] = -1;
    }
}

/**
 * Utility function for reporting the state of the frame array.
 */
void appendFrameState(int *frames, int frameCount, char *frameReport) {
    int i;
    for (i = 0; i < frameCount; i++) {
        char buffer[2];
        if (frames[i] < 0) {
            strncpy(buffer, "-", 2);
        } else {
            sprintf(buffer, "%d", frames[i]);
        }
        strcat(frameReport, buffer);
    }
    strcat(frameReport, "|");
}

/**
 * Utility function for concluding activity on a frame report.
 */
void finishFrameReport(char *frameReport) {
    // Eliminate the trailing "|" in the frame report.
    frameReport[strlen(frameReport) - 1] = '\0';
}

/**
 * Utility function for returning the page at the given index.
 */
int getReference(const char *refString, int currentReference) {
    // Semi-cheat...take advantage of ASCII encoding.
    return (int)refString[currentReference] - (int)'0';
}

/**
 * Returns the frame that holds the given page; -1 if not there.
 */
int getPageFrame(int page, int *frames, int frameCount) {
    int i;
    for (i = 0; i < frameCount; i++) {
        if (frames[i] == page) return i;
    }
    
    // If we get here, then the page was not found.
    return -1;
}

/**
 * Utility function for returning an available frame; returns
 * -1 if no frame is available.
 */
int getAvailableFrame(int *frames, int frameCount) {
    int i;
    for (i = 0; i < frameCount; i++) {
        if (frames[i] == -1) return i;
    }
    
    // If we get here, nothing is available.
    return -1;
}

/**
 * FIFO page replacement implementation.
 */
struct fifo_queue {
    int page;
    struct fifo_queue *next;
};

/**
 * The head of the FIFO queue.
 */
static struct fifo_queue *fifo_head;

/**
 * The tail of the FIFO queue --- avoids searching.
 */
static struct fifo_queue *fifo_tail;

void initFIFOPageReplacement() {
    // TODO
}

void serviceFIFOPageFault(int page) {
    // TODO
}

int getFIFOVictimPage(int *frames, int frameCount) {
    // TODO
}

void useFIFOPage(int page) {
    // No-op; page use doesn't matter in FIFO.
}

void cleanUpFIFO() {
    // TODO
}

/**
 * LRU page replacement implementation.
 */
static int pageAccess[10]; // Per the SGG exercise, we have a maximum of 10 pages.
static int accessCounter;

void initLRUPageReplacement() {
    // TODO
}

void serviceLRUPageFault(int page) {
    // No-op; we don't do anything here with LRU.
}

int getLRUVictimPage(int *frames, int frameCount) {
    // TODO
}

void useLRUPage(int page) {
    // TODO
}

void cleanUpLRU() {
    // No-op; nothing to clean up when using LRU with counters.
}

/**
 * Common framework for page replacement, with divergent functions
 * broken out.
 */
void replacePages(const char *refString, int frameCount, char *frameReport, void (*initPageReplacement)(void),
        void (*servicePageFault)(int), int (*getVictimPage)(int *, int), void (*usePage)(int), void (*cleanUp)(void)) {
    // The simulated physical memory.
    int frames[frameCount];
    initializeFrames(frames, frameCount);
    
    // Initialize the frame report.
    strncpy(frameReport, "", 1);
    
    // *** Initialize any needed page replacement data structures.
    initPageReplacement();
    
    // Iterate through the reference string.
    int currentReference = 0;
    int referenceCount = strlen(refString);
    while (currentReference < referenceCount) {
        // First, report on the current state of things.
        appendFrameState(frames, frameCount, frameReport);
        
        // Grab the page reference and see if it's in memory.
        int page = getReference(refString, currentReference);
        if (getPageFrame(page, frames, frameCount) == -1) {
            // *** Page fault; fetch the page.
            servicePageFault(page);
            
            // Allocate a frame.
            int frame = getAvailableFrame(frames, frameCount);
            if (frame == -1) {
                // *** Not enough frames; replace a page.
                int victim = getVictimPage(frames, frameCount);
                frame = getPageFrame(victim, frames, frameCount);
            }
            frames[frame] = page;
        }
        
        // *** "Access" the page.
        usePage(page);

        // Move to the next reference.
        currentReference++;
    }
    
    // Issue one last report, and conclude it.
    appendFrameState(frames, frameCount, frameReport);
    finishFrameReport(frameReport);
    
    // *** Perform any necessary clean-up.
    cleanUp();
}

void replacePagesFIFO(const char *refString, int frameCount, char *frameReport) {
    replacePages(refString, frameCount, frameReport, initFIFOPageReplacement,
            serviceFIFOPageFault, getFIFOVictimPage, useFIFOPage, cleanUpFIFO);
}

void replacePagesLRU(const char *refString, int frameCount, char *frameReport) {
    replacePages(refString, frameCount, frameReport, initLRUPageReplacement,
            serviceLRUPageFault, getLRUVictimPage, useLRUPage, cleanUpLRU);
}
