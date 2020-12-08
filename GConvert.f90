!find which lines are comment lines
!find embedded comment (need to rule out char-context)
!move embedded comments to next line
!move past-72 code to new line, if it isnt in a comment



!check for comment lines + change to multiform
!on remaining lines,check for past-72 characters and move them to new !-comment line

!on these lines, check col 6 for 0 and change to blank
!on remaining lines, check for continuation, change to &-mark
!


!


!embedded comments: check character context character by character and detect non-cc 
! !-marks

    Module texttools
      Implicit None

      Type sourcelines
        Character (Len=132) :: line, linetype
!  Logical :: charcont(132)
        Integer :: plingcom
      End Type

      Type (sourcelines), Allocatable :: orig(:), edit(:)
      Integer :: tot_lines, tot_edit_lines

    Contains

      Subroutine get_text(a, b) ! stores original source in an array
        Type (sourcelines), Allocatable :: a(:), b(:)
        Character (Len=80) :: filename
        Integer :: i

       ! filename = './test_cases/con_lines.txt'
       ! print *,filename
        
        
      !  Open (Unit=9, File=filename)

        tot_lines = 0
        Do
          Read (*, '(A)', End=100)
          tot_lines = tot_lines + 1
        End Do
100     Continue
        Rewind 5

        Allocate (a(tot_lines), b(2*tot_lines))

        Do i = 1, tot_lines
          Read (*, '(A)') a(i)%line
          Print *, a(i)%line
        End Do

      End Subroutine


      Subroutine find_com_lns(a)
        Type (sourcelines) :: a(:)
        Integer :: i, j

line:   Do i = 1, tot_lines

          If (a(i)%line(1:1)=='!' .Or. a(i)%line(1:1)=='*' .Or. &
            a(i)%line(1:1)=='C' .Or. a(i)%line(1:1)=='c') Then
            
            a(i)%line(1:1) = "!"
            a(i)%linetype = 'cmtline'
            Cycle line

          Else If (a(i)%line==' ') Then
            a(i)%linetype = 'cmtline'
            Cycle line

          Else
charac:     Do j = 2, 132

              If (a(i)%line(j:j)==' ') Then
                Cycle charac
              Else
                If (a(i)%line(j:j)=='!') Then
                  If (j/=6) Then
                    a(i)%linetype = 'cmtline'
                    Cycle line
                  Else
                    a(i)%linetype = 'code'
                    Cycle line
                  End If
                Else
                  a(i)%linetype = 'code'
                  Cycle line
                End If
              End If

            End Do charac

          End If
        End Do line

      End Subroutine


      Subroutine find_emb_com(a)
        Type (sourcelines) :: a(:)
        Integer :: i, j
        Logical :: charnow
        Character :: quotecharac


        charnow = .False. !begin not in character context





line:   Do i = 1, tot_lines

          a(i)%plingcom = -1

          If (a(i)%linetype=='cmtline') Then !skip comment lines
            Cycle line
          End If



!  a(i)%charcont = .false. !initialise all charcters to be not
!in character context

charac:   Do j = 7, 72 ! look in code field


            If (.Not. charnow) Then ! if not in char context

              If (a(i)%line(j:j)=='''' .Or. a(i)%line(j:j)=='"') Then
                quotecharac = a(i)%line(j:j)
                charnow = .True.
                Cycle charac
!quotes when not in char context  puts us in char context

              Else If (a(i)%line(j:j)=='!') Then
                a(i)%plingcom = j

                Cycle line
!!-mark when not in character context means an embedded comment, so we skip
!the rest of the line

              Else
                Cycle charac

              End If
            Else !if in char context

              If (a(i)%line(j:j)==quotecharac) Then !closequote detected
                charnow = .False.
                Cycle charac
              Else
                charnow = .True.
                Cycle charac
              End If
            End If

          End Do charac
        End Do line

    !    Do i = 1, tot_lines

      !    Print *, '-------------'

       !   Print *, a(i)%plingcom
      !    Do j = 7, 72

      !      Print *, a(i)%line(j:j)

      !    End Do
     !   End Do

      End Subroutine


!go if in character context and find endquote, find how many, set = quoteno = temp
!

      Subroutine move_lines(a, b)

        Type (sourcelines) :: a(:), b(:)
        Integer :: i, j

        j = 1

        Do i = 1, tot_lines

          If ((a(i)%linetype=='cmtline')) Then
!if its a comment, its left alone
            b(j) = a(i)
            j = j + 1
!independantly advancing j by 1, because 1 line in b was produced
!for 1 line in a
            Cycle


          Else If (a(i)%plingcom/=-1) Then

!embedded comment, move to new line
            b(j) = sourcelines(line=a(i)%line(1:a(i)%plingcom-1), &
              linetype='code', plingcom=-1)

!jth line of b reads up to embedded comment. it is code and has no embedded comment

            b(j+1) = sourcelines(line='      '//a(i)%line(a(i)%plingcom:132), &
              linetype='cmtline', plingcom=-1)

            j = j + 2
            Cycle


          Else If ((a(i)%plingcom==-1) .And. a(i)%linetype=='code') Then
            If (a(i)%line(73:132)/=' ') Then

!text past col-72 is moved to a new line as a comment

              b(j) = sourcelines(line=a(i)%line(1:72), linetype='code', &
                plingcom=-1)

              b(j+1) = sourcelines(line='      !'//a(i)%line(73:132), &
                linetype='cmtline', plingcom=-1)

              j = j + 2
              Cycle
            Else !code is copied over unedited
              b(j) = a(i)
              j = j + 1
              Cycle
            End If
          End If
        End Do

        tot_edit_lines = j

! Print *, '***********'
    !    Do i = 1, j
    !      Print *, b(i)%line
    !    End Do

      End Subroutine

! cases: comment line, embedded comment, code that goes past col 72

!is it a comment, is it an emb com, is it past 72



      Subroutine edit_con_lns(b)

        Type (sourcelines) :: b(:)
        Integer :: i, j

lines:  Do i = 1, tot_edit_lines

          If (b(i)%linetype=='code') Then
            If (b(i)%line(6:6)==' ') Then
              Cycle !code that is not a continuation

            Else If (b(i)%line(6:6)=='0') Then
              b(i)%line(6:6) = ' '
              Cycle !code that is not a continuation
            Else !code thta is a continuation
              b(i)%line(6:6) = '&' !set cont. character as an &-mark
              Do j = i - 1, 1, -1 !search preceding lines for non-comment line
                If (b(j)%linetype=='cmtline') Then
                  Cycle
                Else
                  b(j)%line(73:73) = '&' !first non-comment line gets a trailing
!&-mark and exits the loop
                  Cycle lines
                End If
              End Do
            End If
          Else
            Cycle ! if line is a comment, leave it
          End If

        End Do lines

      End Subroutine


    End Module






    Program convert

      Use texttools

      Call get_text(orig, edit) !get text into an array
      Call find_com_lns(orig) !find comment lines
      Call find_emb_com(orig) !find embedded comments
      Call move_lines(orig, edit) !move embedded comment and text past column 72
      Call edit_con_lns(edit)

! call find_con_lines

      Print *, '::::::::::::::::'

      Do i = 1, tot_edit_lines
        Print *, edit(i)%line
      End Do

    End Program
