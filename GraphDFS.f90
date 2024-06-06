program GraphDFS
  implicit none

  integer, parameter :: N = 5
  integer :: i, j
  integer, dimension(N, N) :: adjMatrix
  logical, dimension(N) :: visited

  adjMatrix = reshape( &
    [ 0, 1, 0, 0, 0, &
      1, 0, 1, 1, 0, &
      0, 1, 0, 0, 1, &
      0, 1, 0, 0, 1, &
      0, 0, 1, 1, 0 ], &
    [ N, N ])

  visited = .false.

  call DFS(1)

contains

  recursive subroutine DFS(v)
    integer, intent(in) :: v
    integer :: u

    visited(v) = .true.
    print *, 'Visitando nó:', v

    do u = 1, N
      if (adjMatrix(v, u) == 1 .and. .not. visited(u)) then
        call DFS(u)
      end if
    end do
  end subroutine DFS

end program GraphDFS

