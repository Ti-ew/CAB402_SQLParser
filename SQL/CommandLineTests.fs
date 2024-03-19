module CommandLineTests

// Command line tests.

// See also the unit tests.

open Execution

// Simple Tests ...

Display "select * from people"
Display "select * from postcodes"

// Select Expressions
Display "select name from people"
Display "select name,age from people"
Display "select name,12 from people"
Display "select name,\"Fred\" from people"
Display "select name,-age from people"
Display "select name,+age from people"
Display "select name,age+2 from people"
Display "select name,age-5 from people"
Display "select name,age*2 from people"
Display "select name,age/2 from people"

// without From clause
Display "select 42,\"Hello\""

// operator precedence
Display "select 1 + 2 * 3, 1 - 2 * 3, 1 * 2 + 4, 3 / 2 * 4, (1 + 2) * 3"

// where conditions
Display "select * from people where age < 54"
Display "select * from people where name < \"Fred\""
Display "select * from people where age < postcode"
Display "select * from people where age > 54"
Display "select * from people where age <= 54"
Display "select * from people where age >= 54"
Display "select * from people where age = 54"
Display "select * from people where age != 54"
Display "select * from people where age < 54 AND postcode > 4000"
Display "select * from people where age < 54 OR postcode > 4000"

// ------------------------------------------------------------------------//
// Advanced Tests

// aggregate operations
Display "select MAX(age) from people"
Display "select MIN(age) from people"
Display "select MAX(name) from people"
Display "select MIN(name) from people"
Display "select COUNT(age) from people"
Display "select COUNT(name) from people"
Display "select COUNT(*) from people"

// column aliasing
Display "select name,age as experience from people"
Display "select name,\"Fred\" as nickname from people"
Display "select name,-age as age from people"
Display "select name,+age as age from people"
Display "select name,age+2 as age from people"
Display "select name,age-5 as age from people"
Display "select name,age*2 as age from people"
Display "select name,age/2 as age from people"
Display "select MAX(age) as oldest from people"
Display "select MIN(age) as youngest from people"
Display "select MAX(name) as last from people"
Display "select MIN(name) as first from people"
Display "select COUNT(age) as valid from people"
Display "select COUNT(name) as nameCount from people"
Display "select COUNT(*) as rowCount from people"

Display "select * from people where name LIKE \"A%\""
Display "select * from people where postcode IN (SELECT postcode from postcodes)"
Display "select * from people where postcode NOT IN (SELECT postcode from postcodes)"
Display "select * from people where NOT (age < 54 OR postcode > 4000)"

// Group by ...
Display "select age, COUNT(*) as count, MIN(name), MAX(postcode) from people group by age"
Display "select age, state, COUNT(name) as people, MIN(name) from people INNER JOIN postcodes ON people.postcode = postcodes.postcode group by age, state"

// Having ...
Display "select age, COUNT(*) as count, MIN(name), MAX(postcode) from people group by age having COUNT(*) > 1 AND age > 50"
Display "select age, state, COUNT(name) as people, MIN(name) from people INNER JOIN postcodes ON people.postcode = postcodes.postcode group by age, state having count(*) > 1"

Display "select age, COUNT(*) as count, MIN(name), MAX(postcode) from people group by age having COUNT(*) > 1 AND age > 50"

// joins ...
Display "select * from people INNER JOIN postcodes ON people.postcode = postcodes.postcode"
Display "select * from people LEFT JOIN postcodes ON people.postcode = postcodes.postcode"
Display "select * from people RIGHT JOIN postcodes ON people.postcode = postcodes.postcode"
Display "select * from people CROSS JOIN postcodes"
Display "select * from people, postcodes"

Display "select distinct c1.state as state, p1.name as person1, p2.name as person2 from people as p1 INNER JOIN postcodes as c1 ON p1.postcode = c1.postcode INNER JOIN postcodes as c2 ON c1.state = c2.state INNER JOIN people as p2 ON c2.postcode = p2.postcode AND p1.name < p2.name"

// table aliasing
Display "select * from people as p1, people as p2 where p1.postcode = p2.postcode"
Display "select p1.name as first, p2.name as second, p1.postcode as postcode from people as p1, people as p2 where p1.postcode = p2.postcode and p1.name != p2.name"

// table.*
Display "select people.name,postcodes.* from people INNER JOIN postcodes ON people.postcode = postcodes.postcode"

//order by
Display "select * from people order by name"
Display "select * from people order by age"
Display "select * from people order by name,age"
Display "select * from people order by age,name"

//limit
Display "select * from people limit 50"
Display "select * from people limit 5"
Display "select * from people limit 1"
Display "select * from people limit 0"

Display "select * from BigTable limit 10"

// Big Data!

Display "select * from BigTable where PostCode = 4054 limit 10"