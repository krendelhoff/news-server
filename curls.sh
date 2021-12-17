curl "localhost:3000/users/create" \
   -X POST
   --data '{"name":"Savely","surname":"Krendelhoff","login":"krendelhoff","password":"test123"}'

curl "localhost:3000/auth/login" \
     -X POST
     --data '{"login":"krendelhoff","password":"test123"}'

curl "localhost:3000/users" \
  -H "Authorization: Bearer f859245f78addaae96b5d12a736684bf42e924642132d924b66b1d72a010a0fb"
