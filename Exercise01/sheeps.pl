:- module(sheeps).

female(dolly).
female(haba).
female(doerte).
female(pauline).

male(friedrich).
male(gunter).
male(peter).

parent(haba,dolly).
parent(haba,gunter).
parent(friedrich,dolly).
parent(friedrich,gunter).
parent(gunter,peter).
parent(gunter,pauline).
parent(doerte,peter).
parent(doerte,pauline).

% Schreiben Sie ein Praedikat sheep/1, dass fuer alle Schafe in der Datenbank wahr ist.
sheep(X) :- female(X) ; male(X).
% Schreiben Sie zwei Praedikate father/2 und mother/2. Die Anfrage father(F,C) soll genau dann wahr
% sein, wenn F Vater von C ist (mother/2 analog).
father(F,C) :- male(F) , parent(F,C).
mother(M,C) :- female(M) , parent(M,C).
% Schreiben Sie ein Praedikat ancestor/2. Die Anfrage ancestor(A,D) soll genau dann wahr sein, wenn A
% eine Vorfahrin bzw. ein Vorfahr von D ist.
ancestor(A,D) :- parent(A,D).
ancestor(A,D) :- parent(A,C) , ancestor(C,D).
