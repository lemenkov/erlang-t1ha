%% Copyright (c) 2016 Peter Lemenkov, <lemenkov@gmail.com>,
%% Fast Positive Hash written in Erlang.
%%
%% Portions Copyright (c) 2010-2013 Leonid Yuriev <leo@yuriev.ru>,
%% The 1Hippeus project (t1h).
%%
%% This software is provided 'as-is', without any express or implied
%% warranty. In no event will the authors be held liable for any damages
%% arising from the use of this software.
%%
%% Permission is granted to anyone to use this software for any purpose,
%% including commercial applications, and to alter it and redistribute it
%% freely, subject to the following restrictions:
%%
%% 1. The origin of this software must not be misrepresented; you must not
%%    claim that you wrote the original software. If you use this software
%%    in a product, an acknowledgement in the product documentation would be
%%    appreciated but is not required.
%% 2. Altered source versions must be plainly marked as such, and must not be
%%    misrepresented as being the original software.
%% 3. This notice may not be removed or altered from any source distribution.

-module(t1ha).

%% t1ha: t1ha library's entry point.

-export([hash/2]).

% Magic Primes
-define(P0, 17048867929148541611).
-define(P1, 9386433910765580089).
-define(P2, 15343884574428479051).
-define(P3, 13662985319504319857).
-define(P4, 11242949449147999147).
-define(P5, 13862205317416547141).
-define(P6, 14653293970879851569).

% rotations
-define(S0, 41).
-define(S1, 17).
-define(S2, 31).

%% API

hash(Binary, Seed) ->
	hash(Binary, Seed, Seed, size(Binary)).

%% Internals

hash(Binary, Seed, A, B) when 32 < size(Binary) ->
	C = lo(rot64(size(Binary), ?S1) + Seed),
	D = size(Binary) bxor rot64(Seed, ?S1),

	{Rest, NewA, NewB, NewC, NewD} = skip_until_less_32(Binary, A, B, C, D),

	NextA = NewA bxor lo(?P6 * lo(rot64(NewC, ?S1) + NewD)),
	NextB = NewB bxor lo(?P5 * lo(NewC + rot64(NewD, ?S1))),

	hash(Rest, Seed, NextA, NextB);

hash(<<Int64:64/little-unsigned-integer, Rest/binary>> = Binary, Seed, A, B) when 25 =< size(Binary), size(Binary) =< 32 ->
	NewB = lo(B + mux64(Int64, ?P4)),
	hash(Rest, Seed, A, NewB);

hash(<<Int64:64/little-unsigned-integer, Rest/binary>> = Binary, Seed, A, B) when 17 =< size(Binary), size(Binary) =< 24 ->
	NewA = lo(A + mux64(Int64, ?P3)),
	hash(Rest, Seed, NewA, B);

hash(<<Int64:64/little-unsigned-integer, Rest/binary>> = Binary, Seed, A, B) when 9 =< size(Binary), size(Binary) =< 16 ->
	NewB = lo(B + mux64(Int64, ?P2)),
	hash(Rest, Seed, A, NewB);

hash(Binary, Seed, A, B) when 1 =< size(Binary), size(Binary) =< 8 ->
	% http://erlang.org/pipermail/erlang-questions/2007-August/028344.html
	Bin2Int = fun (Bin) -> L = 8 * size(Bin), <<Int:L/little-unsigned-integer>> = Bin, Int end,
	NewA = lo(A + mux64(Bin2Int(Binary), ?P1)),
	hash(<<>>, Seed, NewA, B);

hash(<<>>, _Seed, A, B) ->
	Ret = mux64(rot64(lo(A + B), ?S1), ?P4) + mix(A bxor B, ?P0),
	<<Ret:64/big-unsigned-integer>>.

skip_until_less_32(<<W0:64/little-unsigned-integer, W1:64/little-unsigned-integer, W2:64/little-unsigned-integer, W3:64/little-unsigned-integer, Rest/binary>> = Binary, A, B, C, D) when 32 =< size(Binary) ->
	D02 = W0 bxor rot64(lo(W2 + D), ?S1),
	C13 = W1 bxor rot64(lo(W3 + C), ?S1),

	NewA = A bxor lo(?P1 * lo(D02 + W3)),
	NewB = B bxor lo(?P0 * lo(C13 + W2)),
	NewC = lo(C + (A bxor rot64(W0, ?S0))),
	NewD = lo(D - (B bxor rot64(W1, ?S2))),

	skip_until_less_32(Rest, NewA, NewB, NewC, NewD);
skip_until_less_32(Binary, A, B, C, D) ->
	{Binary, A, B, C, D}.

%% Internal primitives

mux64(A, B) ->
	Ret = A * B,
	lo(Ret) bxor hi(Ret).

% xor-mul-xor mixer
mix(V, P) ->
	Ret = lo(V * P),
	Ret bxor rot64(Ret, ?S0).

lo(V) ->
	V band (1 bsl 64 - 1).
hi(V) ->
	V bsr 64.

% 64-bit
rot64(V, Shift) ->
	lo((V bsr Shift) bor (V bsl (64 - Shift))).

%% End of Module.
