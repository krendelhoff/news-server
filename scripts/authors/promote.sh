curl "localhost:7980/authors" \
     -X POST \
     --data '{"userId": "b6fee889-1b35-4ff7-a70a-947c8e796d6b", "description": "The king of Development"}' \
     -H "Authorization: Bearer 517ad09dfdceba158d7d0a0580fcbb355b6e44cb608e255e31b4fd4489164842"
