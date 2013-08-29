      PROGRAM MORT

C Calculate the length of a morgage in months 
C OUTS - outstanding amount 
C INTR - interest charged
C RATE - monthly interest rate 
C N - number of months
C REPAY - Repayment
      REAL OUTS, INTR, RATE, REPAY
      INTEGER N

C Initial values
      OUTS = 10000.0
      RATE = (6.15 / 100.0) / 12.0
      REPAY = 200.0
      N = 0

10    INTR = OUTS * RATE
      OUTS = OUTS + INTR
      OUTS = OUTS - REPAY
      N = N + 1
      IF (OUTS .GT. 0.0) GO TO 10

20    WRITE (6, FMT=100) N
100   FORMAT (I5)

      CLOSE(6)
      STOP
      END
