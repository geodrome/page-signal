package com.github.geodrome.wordmatcher;

public class WordMatcher {


    /* Takes two arrays of strings and calculates the number of words matching.
    Starts by finding the longest contiguous matching sequence.  The same idea is then
    recursively applied to the pieces of the sequences to the left and to the right
    of the matching subsequence

     */

    /* an optimization to stop when max-len is greater than what is
remaining in the arrays? Only makes sense where long matches are expected */

    public static int count(String[] a1, String[] a2) {
        return countMatches(a1, 0, a1.length, a2, 0, a2.length);
    }

    public static int countMatches(String[] a1, int a1Start, int a1End,
                                   String[] a2, int a2Start, int a2End) {
        Match match = longestContigMatch(a1, a1Start, a1End, a2, a2Start, a2End);
        if (match.len == 0) { return 0; }

        int left = 0;
        int right = 0;

        // calculate left
        int a1LeftEnd = match.a1Last - match.len + 1;
        int a2LeftEnd = match.a2Last - match.len + 1;
        if (a1LeftEnd > match.a1Start && a2LeftEnd > match.a2Start) {
            left = countMatches(a1, match.a1Start, a1LeftEnd,
                    a2, match.a2Start, a2LeftEnd);
        }

        // calculate right
        int a1RightStart = match.a1Last + 1;
        int a2RightStart = match.a2Last + 1;
        if (a1RightStart < match.a1End && a2RightStart < match.a2End) {
            right = countMatches(a1, a1RightStart, match.a1End,
                    a2, a2RightStart, match.a2End);
        }

        return match.len + left + right;
    }

    public static Match longestContigMatch(String[] a1, int a1Start, int a1End,
                                           String[] a2, int a2Start, int a2End) {
        Match match = new Match(0, -1, -1, a1Start, a1End, a2Start, a2End);

        if (a1 == null || a2 == null || a1Start > a1End || a2Start > a2End) {
            return match;
        }

        int sz = a2End - a2Start + 1;
        int[] prev = new int[sz]; // holds data from previous iteration of inner for loop
        int[] curr = new int[sz]; // used for the 'current' iteration of inner for loop

        int len = 0; // holds current match length

        for (int i = a1Start; i < a1End; ++i) {
            for (int j = a2Start; j < a2End; ++j) {
                if (a1[i].equals(a2[j])) {
                    len = prev[j - a2Start] + 1;
                    // curr and prev are padded by 1 to allow for this assignment when j=0
                }
                else {
                    len = 0;
                }
                curr[j - a2Start + 1] = len;

                if (len > match.len) {
                    match.len = len;
                    match.a1Last = i;
                    match.a2Last = j;
                }
            }

            int[] swap = prev;
            prev = curr;
            curr = swap;
        }
        return match;
    }
}