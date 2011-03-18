% Use these macros to write/read numbers from bson or mongo binary format

-define (put_int32 (N), (N):32/signed-little).
-define (put_int64 (N), (N):64/signed-little).
-define (put_float (N), (N):64/float-little).
-define (put_bits32 (B7,B6,B5,B4,B3,B2,B1,B0), (B7):1,(B6):1,(B5):1,(B4):1,(B3):1,(B2):1,(B1):1,(B0):1,0:24).

-define (get_int32 (N), N:32/signed-little).
-define (get_int64 (N), N:64/signed-little).
-define (get_float (N), N:64/float-little).
-define (get_bits32 (B7,B6,B5,B4,B3,B2,B1,B0), B7:1,B6:1,B5:1,B4:1,B3:1,B2:1,B1:1,B0:1,_:24).
