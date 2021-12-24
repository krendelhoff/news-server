curl "localhost:3000/users/create" \
   -X POST \
   --data '{"name":"Savely","surname":"Krendelhoff","login":"krendelhoff","password":"test123"}'

curl "localhost:3000/auth/login" \
     -X POST \
     --data '{"login":"krendelhoff","password":"test123"}'

curl "localhost:3000/users" \
  -H "Authorization: Bearer 0c25e47984d77fbb28fed673a65579b5290af4c802b896c338a48ec582079af0"
