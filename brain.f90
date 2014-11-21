PROGRAM brainfuck
  INTEGER, DIMENSION(30000) :: tape = 0
  CHARACTER(LEN=255) :: code
  INTEGER :: loop_start, loop_tape = 0, j = 15000, i = 0

  code = '++++++++++[>+++++++<-]>.'

  DO
    i = i + 1
    IF (i .GT. LEN(TRIM(code)) .OR. code(i:i) .EQ. '') EXIT
    IF (code(i:i) .EQ. '>') THEN
      j = j + 1
    ELSE IF (code(i:i) .EQ. '<') THEN
      j = j - 1
    ELSE IF (code(i:i) .EQ. '.') THEN
      PRINT '(A)', CHAR(tape(j))
    ELSE IF (code(i:i) .EQ. '-') THEN
      tape(j) = tape(j) - 1
      IF (tape(j) .LT. 0) THEN
        tape(j) = 255
      END IF
    ELSE IF (code(i:i) .EQ. '+') THEN
      tape(j) = tape(j) + 1
      IF (tape(j) .GT. 255) THEN
        tape(j) = 0
      END IF
    ELSE IF (code(i:i) .EQ. '[') THEN
      loop_start = i-1
      loop_tape = j
    ELSE IF (code(i:i) .EQ. ']' .AND. tape(loop_tape) .GT. 0) THEN
      i = loop_start
    END IF
  END DO
END PROGRAM brainfuck
