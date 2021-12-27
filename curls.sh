curl "localhost:3000/users/create" \
   -X POST \
   --data '{"name":"Savely","surname":"Krendelhoff","login":"krendelhoff","password":"test123"}'

curl "localhost:3000/auth/login" \
     -X POST \
     --data '{"login":"krendelhoff","password":"test123"}'

curl "localhost:3000/users" \
  -H "Authorization: Bearer 73d5cb0cb94021202cac78b9715fbfc776ca8039d5ef0683a02afea0e709c7cf"

curl -k -F 'image=@./pikachu.png' -v 'localhost:3000/pictures' \
     -H "Authorization: Bearer 0c25e47984d77fbb28fed673a65579b5290af4c802b896c338a48ec582079af0"

curl "localhost:3000/auth/refresh/b3050d8794a45b66cd7f6a4107eb52ee39aa6db27e1d9802a1fa120dc24a39b6" \
     -H "Authorization: Bearer 73d5cb0cb94021202cac78b9715fbfc776ca8039d5ef0683a02afea0e709c7cf"
