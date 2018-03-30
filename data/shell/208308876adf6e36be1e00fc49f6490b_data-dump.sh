x=`date +%d%m%Y`
mysqldump  --no-create-info --complete-insert --skip-extended-insert --skip-triggers -u root -p webgloo250710 > webgloodb.data.$x.sql
