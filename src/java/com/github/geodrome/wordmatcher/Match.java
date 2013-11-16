package com.github.geodrome.wordmatcher;

public class Match {
    public Match(int l, int a1l, int a2l,
                 int a1S, int a1E, int a2S, int a2E) {
        len = l;
        a1Last = a1l;
        a2Last = a2l;

        a1Start = a1S;
        a1End = a1E;
        a2Start = a2S;
        a2End = a2E;
    }
    public int len; public int a1Last; public int a2Last;
    public int a1Start; public int a1End; public int a2Start; public int a2End;
}
