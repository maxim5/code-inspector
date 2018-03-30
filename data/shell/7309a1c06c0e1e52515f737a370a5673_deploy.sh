application=doveProxy
org=demox
environment=prod
credentials=ps@apigee.com:ps@apigee
url=https://api.enterprise.apigee.com

echo find . -name .DS_Store -print0 | xargs -0 rm -rf
find . -name .DS_Store -print0 | xargs -0 rm -rf

#un-deploy and delete the older version (verion 1)

echo curl -u $credentials "$url/v1/organizations/$org/apis/$application/revisions/1/deployments?action=undeploy&env=$environment" -X POST -H "Content-Type: application/octet-stream"


curl -u $credentials "$url/v1/organizations/$org/apis/$application/revisions/1/deployments?action=undeploy&env=$environment" -X POST -H "Content-Type: application/octet-stream"

echo curl -u $credentials -X DELETE "$url/v1/organizations/$org/apis/$application/revisions/1"

curl -u $credentials -X DELETE "$url/v1/organizations/$org/apis/$application/revisions/1"


rm -rf $application.zip

#Create the bundle and deploy
zip -r $application.zip apiproxy

echo curl -v -u $credentials "$url/v1/organizations/$org/apis?action=import&name=$application" -T $application.zip -H "Content-Type: application/octet-stream" -X POST

curl -v -u $credentials "$url/v1/organizations/$org/apis?action=import&name=$application" -T $application.zip -H "Content-Type: application/octet-stream" -X POST

#Activate
echo curl -u $credentials "$url/v1/organizations/$org/apis/$application/revisions/1/deployments?action=deploy&env=$environment" -X POST -H "Content-Type: application/octet-stream"


curl -u $credentials "$url/v1/organizations/$org/apis/$application/revisions/1/deployments?action=deploy&env=$environment" -X POST -H "Content-Type: application/octet-stream"
