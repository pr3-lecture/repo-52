# Triangle Project Code.

# Triangle analyzes the lengths of the sides of a triangle
# (represented by a, b and c) and returns the type of triangle.
#
# It returns:
#   :equilateral  if all sides are equal
#   :isosceles    if exactly 2 sides are equal
#   :scalene      if no sides are equal
#
# The tests for this method can be found in
#   about_triangle_project.rb
# and
#   about_triangle_project_2.rb
#
def triangle(a, b, c)
  raise TriangleError, "Alle Seite müssen positiv sein" if [a,b,c].map { |x| x <= 0 }.all?
  raise TriangleError, "Fehler" if ( a + b + c - ( 2 *  [a,b,c].max ) <= 0  )
  case [a,b,c].uniq.size
    when 1 then :equilateral
    when 2 then :isosceles
    when 3 then :scalene
  end
end

# Error class used in part 2.  No need to change this code.
class TriangleError < StandardError
end
