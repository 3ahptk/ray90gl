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

    type sphere
      real, dimension(0:2)  :: center
      real :: radius
    end type sphere

    type camera
      real, dimension(0:2) :: origin
      real, dimension(0:2) :: lower_left_corner
      real, dimension(0:2) :: horizontal
      real, dimension(0:2) :: vertical
      real :: scale
    end type camera
    
    integer :: handle
    
    call glutinit
    call glutinitdisplaymode(ior(GLUT_SINGLE,GLUT_RGB))
    call glutInitWindowSize(500_glcint,500_glcint)
    call glutInitWindowPosition (0_glcint, 0_glcint)
    handle = glutCreateWindow ("Raytrace Sphere Test")
    !call glutSpecialFunc(processNormalKeys)
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
    
         !select case (key)
         !  case(iachar('W'),iachar('w'))  !w 
         !    call turn_y(vertical,-PI/16.)
         !end select
        end subroutine processNormalKeys

        subroutine get_camera_ray(u, v, c, o)
          real,intent(in)  :: u
          real,intent(in)  :: v
          type(camera),intent(in) :: c
          type(ray),intent(out) :: o
          o%origin = c%origin
          o%direction = c%lower_left_corner + u*c%horizontal + v*c%vertical - c%origin           
        end subroutine get_camera_ray
    
        subroutine color(r, reflect, o)
            type(ray),intent(inout) :: r
            logical, intent(in) :: reflect
            logical :: hit_any
            real, dimension(0:2), intent(inout) :: o
            real, dimension(0:2) :: unit_direction, o2, N
            real :: t, h
      
            hit_any = .false.
            hit_record temp_rec = new hit_record()
            hit_record rec = new hit_record()
            float closest = float.MaxValue

            do concurrent(var o in objects)
                if (o.hit(r, 0.001, closest, out temp_rec)) then
                    hit_any = .true.
                    closest = temp_rec.t
                    rec = temp_rec
                end if
            end do

            if (hit_any && reflect>0) then
                vec3 target = rec.p + rec.normal !+ random_unit_in_sphere()
                return 0.5 * color(new ray(rec.p, target - rec.p), --reflect)
            end if

            vec3 unit_dir = vec3.unit_vector(r.direction)
            float t = 0.5 * (unit_dir.y + 1.0)
            return (1.0 - t) * [1.0, 1.0, 1.0] + t * [0.5, 0.7, 1.0]
      
          end subroutine color

        subroutine hit_sphere(r, t_min, t_max, rec, hit)
          type(ray), intent(in) :: r
          real, intent(in):: t_min
          real, intent(in) :: t_max
          type(hit_record), intent(out) rec
          logical, intent(out) :: hit

          real, dimension(0:2) :: oc
          real :: a,b,c,discriminant
          oc = r%origin - center
          a = dot_product(r%direction,r%direction)
          b = 2.0 * dot_product(oc,r%direction)
          c = dot_product(oc,oc) - radius*radius
          discriminant = b * b - a * c
          if (discriminant > 0) then          
              temp = ((-b - sqrt(discriminant)) / a)
              if (temp < t_max && temp > t_min) then             
                  rec%t = temp
                  call point_at_parameter(r,rec%t,rec%p)
                  rec%normal = (rec%p - center) / radius
                  hit = .true.
                  return
                end if
              temp = ((-b + sqrt(discriminant)) / a)
              if (temp < t_max && temp > t_min) then             
                  rec%t = temp
                  call point_at_parameter(r,rec%t,rec%p)
                  rec%normal = (rec%p - center) / radius
                  hit = .true.
                  return
              end if
            end if          
            hit = .false.
        end subroutine hit_sphere

          function vec_length(v) result(f)
            real, dimension(0:2) :: v
            real :: f
            f = sqrt(v(0)*v(0) + v(1)*v(1) + v(2)*v(2))              
          end function length
      
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
    