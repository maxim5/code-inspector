Manage Users<br>
<br><a href="?scm=adduserman" >Add User</a><br><br>
<?php
$query = mysql_query("select * from users");
echo "<table>";
while ($row = mysql_fetch_array($query, MYSQL_NUM)) {
    print("<tr><td>".$row[4]."</td><td>".$row[5]."</td><td><a href='?scm=edituserman&id=".$row[2]."'>".$row[0]."</a></td><td>".$row[3]."</td><td> ".$row[8]."</td><td><a href='?scm=passuserman&id=".$row[2]."'>pass</a></td><td><a href='".$path."changes/deluserman.php?id=".$row[2]."'>delete</a><br></td></tr>");  
}