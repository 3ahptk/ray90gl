program raygl

    use opengl_gl
    use opengl_glu
    use opengl_glut
    
    implicit none
    
    type ray
    real, dimension(0:2) :: origin
    real, dimension(0:2) :: direction
    end type ray

    type hit_record
      real :: t
      real, dimension(0:2) :: p
      real, dimension(0:2) :: normal
    end type hit_record
    
    integer, parameter   :: nx = 500
    integer, parameter   :: ny = 500  
    integer              :: i, j, ir, ig, ib, dummy, inp = 0
    real                 :: u, v, PI
    character            :: c
    real, dimension(0:2) :: col,lower_left_corner,horizontal,vertical,origin,forwards_back,left_right
    type(ray) :: r
    integer :: handle
    
    call glutinit
    call glutinitdisplaymode(ior(GLUT_SINGLE,GLUT_RGB))
    call glutInitWindowSize(500_glcint,500_glcint)
    call glutInitWindowPosition (0_glcint, 0_glcint)
    handle = glutCreateWindow ("Raytrace Sphere Test")
    call glutSpecialFunc(processNormalKeys)
    call glutDisplayFunc(display)
    call glutIdleFunc(display)
    call myinit
    call glutMainLoop
    
    contains
    
        subroutine myinit
          call glClearColor(1.0_glclampf, 1.0_glclampf, 1.0_glclampf, 1.0_glclampf)
          call glColor3f(1.0_glclampf, 0.0_glclampf, 0.0_glclampf)
          call glMatrixMode(GL_PROJECTION)
          call glLoadIdentity
          call gluOrtho2D(0.0_gldouble, 500.0_gldouble, 0.0_gldouble, 500.0_gldouble)
          call glMatrixMode(GL_MODELVIEW)
    
          PI = 4*atan(1.)
          lower_left_corner = [-2.0, -1.0, -1.0]
          horizontal = [4.0, 0.0, 0.0]
          vertical = [0.0, 2.0, 0.0]
          origin = [0.0, 0.0, 0.0]
    
        end subroutine myinit
    
        subroutine display 
            
            do j = ny-1, 0, -1
                do i = 1, nx, 1
                  u = real(i)/real(nx)
                  v = real(j)/real(ny)
                  r%origin = origin
                  r%direction = lower_left_corner + u*horizontal + v*vertical
                  call color(r, col)
                  call glBegin(GL_POINTS)
                  ir = int(255.99*col(0))
                  ig = int(255.99*col(1))
                  ib = int(255.99*col(2))
                  call glColor3f(real(ir)/255.0, real(ig)/255.0, real(ib)/255.0)
                  call glVertex3f(real(i), real(j), 0.0_glfloat)
                  call glEnd
                end do
              end do
              call glFlush
        end subroutine display
    
        subroutine processNormalKeys(key,x,y)
    
          integer(kind=glint) , intent(in out) :: key
          integer(kind=glcint) , intent(in out) :: x, y
    
          call cross(forwards_back,horizontal,vertical)
          call norm(forwards_back)
          call cross(left_right,vertical,forwards_back)
          call norm(left_right)
    
          select case (key)
            case(iachar('W'),iachar('w'))  !w 
              origin = origin - forwards_back/16.
            case(iachar('A'),iachar('a'))  !a
              origin = origin - left_right/16.
            case(iachar('S'),iachar('s'))  !s
              origin = origin + forwards_back/16.
            case(iachar('D'),iachar('d'))  !d
              origin = origin + left_right/16.
            case(iachar('Q'),iachar('q'))  !q
              call turn_y(lower_left_corner,PI/16.)
              call turn_y(horizontal,PI/16.)
              call turn_y(vertical,PI/16.)
            case(iachar('E'),iachar('e'))  !e
              call turn_y(lower_left_corner,-PI/16.)
              call turn_y(horizontal,-PI/16.)
              call turn_y(vertical,-PI/16.)
          end select
        end subroutine processNormalKeys
    
        subroutine color(r, o)
            type(ray),intent(inout) :: r
            real, dimension(0:2), intent(inout) :: o
            real, dimension(0:2) :: unit_direction, o2, N
            real :: t, h
      
            call hit_sphere([0.0,0.0,-1.0], 0.5, r, h)
      
            if ( h > 0.0 ) then
              call point_at_parameter(r,h,o2)
              call unit_vector( o2 - [0.0,0.0,-1.0] , N )
              o = 0.5*[N(0)+1, N(1)+1, N(2)+1]
              return
            end if
      
            call unit_vector(r%direction,unit_direction)
            t = 0.5 * unit_direction(1) + 1
            o = (1.0-t) * [1.0,1.0,1.0] + t * [0.5,0.7,1.0]
      
          end subroutine color
      
          subroutine hit_sphere(center, radius, r, hit)
            type(ray), intent(in) :: r
            real, intent(in) :: radius
            real, dimension(0:2), intent(in) :: center
            real, intent(inout) :: hit
            real, dimension(0:2) :: oc
            real :: a,b,c,discriminant
            oc = r%origin - center
            a = dot_product(r%direction,r%direction)
            b = 2.0 * dot_product(oc,r%direction)
            c = dot_product(oc,oc) - radius*radius
            discriminant = b*b - 4*a*c
            if (discriminant < 0) then
              hit = -1.0
            else
              hit = (-b - sqrt(discriminant)) / (2.0*a)
            end if
          end subroutine hit_sphere
      
          subroutine unit_vector(v,v2)
            real, dimension(0:2), intent(in) :: v
            real, dimension(0:2), intent(out) :: v2
            v2 = v / sqrt(v(0)*v(0) + v(1)*v(1) + v(2)*v(2))
          end subroutine unit_vector
      
          subroutine point_at_parameter(r,t,o)
            type(ray),intent(inout) :: r
            real, intent(in) :: t
            real, dimension(0:2), intent(inout) :: o
            o = r%origin + t*r%direction
          end subroutine point_at_parameter
    
          subroutine turn_x(v, d)
              real, dimension(0:2),intent(inout) :: v
              real, intent(in) :: d
              v(1) = cos(d) * v(1) + (-(sin(d))) * v(2)
              v(2) = sin(d) * v(1) + cos(d) * v(2)
          end subroutine
      
          subroutine turn_y(v, d)
              real, dimension(0:2),intent(inout) :: v
              real, intent(in) :: d
              v(0) = cos(d) * v(0) + sin(d) * v(2)
              v(2) = (-(sin(d))) * v(0) + cos(d) * v(2)
          end subroutine turn_y
      
          subroutine turn_z(v, d)
              real, dimension(0:2),intent(inout) :: v
              real, intent(in) :: d
              v(0) = cos(d) * v(0) + (-(sin(d))) * v(1)
              v(1) = sin(d) * v(0) + cos(d) * v(1)
          end subroutine turn_z
      
          subroutine norm(vec3)
            real,dimension(0:2),intent(inout) :: vec3
            vec3(0) = vec3(0)/norm2(vec3)
            vec3(1) = vec3(1)/norm2(vec3)
            vec3(2) = vec3(2)/norm2(vec3)
          end subroutine norm
      
          subroutine cross(cp, a, b)
            real,dimension(0:2),intent(out) :: cp
            real,dimension(0:2),intent(in) :: a
            real,dimension(0:2),intent(in) :: b
            cp(0) = a(1) * b(2) - a(2) * b(1)
            cp(1) = a(2) * b(0) - a(0) * b(2)
            cp(2) = a(0) * b(1) - a(1) * b(0)
          end subroutine cross
    
    end program raygl
    