curl "localhost:7980/auth/register" \
     -X POST \
     -H "Content-type: application/json" \
     --data '{"name":"Savely","surname":"Krendelhoff","login":"krendelhoff","password":"test123"}'
