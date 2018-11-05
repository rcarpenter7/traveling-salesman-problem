! PROGRAM 4
! TRAVELING SALESMAN PROBLEM IN FORTRAN 95
! CS320 - Fall 2018
! 10/15/18 cssc0504 Robert Carpenter


PROGRAM P4
	
	IMPLICIT NONE

INTEGER :: i,count,distance,col,row,arrayincrementer, readctr,citynamectr,disctr
INTEGER :: ios,permutations,best_distance
INTEGER:: READBOUNDS
CHARACTER(20) :: filename



CHARACTER(20), ALLOCATABLE, DIMENSION(:) :: city_list
INTEGER ,ALLOCATABLE, DIMENSION(:,:) :: d_table
INTEGER, ALLOCATABLE , DIMENSION(:) :: path
INTEGER,ALLOCATABLE, DIMENSION(:) :: distances
CHARACTER(10),ALLOCATABLE,DIMENSION(:) :: city
INTEGER,ALLOCATABLE,DIMENSION (:) :: best_path
PRINT *, "Enter filename"
READ *, filename

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Open the file and read number of cities
OPEN(UNIT=9, FILE=filename, FORM="FORMATTED", ACTION="READ",&
                                     STATUS="OLD",IOSTAT=ios)
IF(ios /= 0) THEN
    PRINT *, "Could not open file ",TRIM(filename)," Error code: ", ios
    STOP
END IF

READ (UNIT=9, FMT=100) count
PRINT *, "Number of cities: ",count

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Allocate memory for all needed arrays
ALLOCATE(city_list(count*count+count),STAT=ios)
IF(ios /= 0) THEN
    PRINT *, "ERROR, could not allocate memory."
    STOP
END IF

ALLOCATE(d_table(count,count))
ALLOCATE(path(count))
ALLOCATE (distances(count*count))
ALLOCATE(best_path(count))
ALLOCATE(city(count))



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Fill in array from data file

! FILL EVERYTHING IN FILE
DO i=1, count*count+count

  READ (UNIT=9,FMT=200) city_list(i)
END DO


! GRAB NUMBERS NOW
disctr=1
DO i=1 ,count*count+count
	IF((readctr-count) /= 0 .AND. i /= 1) THEN
	READ(city_list(i),*) distances(disctr)
	disctr = disctr+1
	readctr=readctr+1
	ELSE
		readctr=0
	END IF
END DO

! PUT NUMBERS IN 2D ARRAY
arrayincrementer=1
   DO col = 1, count                       
        DO row = 1, count 
           d_table(row,col) = distances(arrayincrementer)
	   arrayincrementer= arrayincrementer+1
        END DO 
    END DO


! EXTRACT CITY NAMES
citynamectr=1
DO i=1 , count
	city(i) = city_list(citynamectr)
	citynamectr = citynamectr+(count+1)
END DO


! PRE POPULATE PATH ARRAYS WITH INDEX NUMBERS
DO i=1, count
	path(i) = i
END DO

DO i=1, count
	best_path(i) = i
END DO

	


! SET BEST DISTANCES TO AN OUTLANDISH NUMBER IN ORDER TO START FROM THERE AND WORK DOWN
best_distance = 1000000
call permute(2,count)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! PRINT THE ANSWER :) WOOOOOOOOOOOO TOOK 10 HOURS BUT FINALLY


DO i=1, count
	IF(i==count) THEN
		PRINT*," ",city(best_path(i)),"to ", city(best_path(1)),"-- ",d_table(best_path(i),1),"miles"

	ELSE
		PRINT*," ",city(best_path(i)),"to ", city(best_path(i+1)),"-- ",d_table(best_path(i),best_path(i+1)),"miles"

	END IF
END DO
PRINT*
PRINT*,"This route is the least distance with a total trip of",best_distance,"miles"


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

100 FORMAT (I6)
200 FORMAT (A)

CONTAINS

RECURSIVE SUBROUTINE permute(first, last)
INTEGER, INTENT(IN) :: first, last
INTEGER :: i, temp
         
	IF(first == last)  THEN
		distance = d_table(1,path(2))
				DO i=2, last-1           
			distance = distance + d_table(path(i),path(i+1)) 
					END DO
		distance = distance + d_table(path(last),path(1))
				permutations = permutations + 1
		IF(distance < best_distance) THEN
			best_distance = distance
			DO i=2, count
				best_path(i) = path(i)
			END DO
		END IF
		
	ELSE
		DO i=first, last
			temp = path(first)
			path(first) = path(i)
			path(i) = temp

			call permute(first+1,last)
	 
			temp = path(first)
			path(first) = path(i)
			path(i) = temp
			
		END DO
	END IF
	

	



	
END SUBROUTINE permute   



END PROGRAM P4
