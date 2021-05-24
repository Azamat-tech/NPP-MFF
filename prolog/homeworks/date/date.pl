months_with_31(jan).
months_with_31(mar).
months_with_31(may).
months_with_31(jul).
months_with_31(aug).
months_with_31(oct).
months_with_31(dec).

months_with_28(feb).

months_with_30(apr).
months_with_30(jun).
months_with_30(sep).
months_with_30(nov).



all_dates(X, Y, Z) :- member([X,Y,Z],
    [[jan, 31, 1], [feb, 28, 2], [mar, 31, 3], [apr, 30, 4],
     [may, 31, 5], [jun, 30, 6], [jul, 31, 7], [aug, 31, 8],
     [sep, 30, 9], [oct, 31, 10], [nov, 30, 11], [dec, 31, 12]]).

add_previous_months(_,31,1).
add_previous_months(_, R, O) :- all_dates(_, X, O),
                                O in 1..11, O1 #= O - 1,
                                add_previous_months(_,R1,O1),
                                R #= X + R1.

convert(date(D, M), R) :- all_dates(M,_,O), O1 #= O - 1,
                          O1 > 0,
                          add_previous_months(M, MR, O1),
                          ((months_with_31(M), D in 1..31 );
                          (months_with_28(M), D in 1..28);
                          (months_with_30(M), D in 1..30)),
                           R #= MR + D.

convert(date(D,M), D) :-  all_dates(M, _, O), O1 #= O - 1,
                          O1 = 0, D in 1..31.

add_date(D, N, Q) :- convert(D, DR), convert(Q, QR),
                     QR #= DR + N.
