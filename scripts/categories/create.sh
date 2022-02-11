curl -v "localhost:7980/categories" \
  -X POST \
  -H "Content-type: application/json" \
  -H "Authorization: Bearer 07cfd94cae21f2ad46307c6062074ea28b87e17e559b4c2f48682094ffd16b2a" \
  --data '{"title": "Cartesian closed", "parent": "706bb670-daab-4bf3-9b23-dcfb31078d1e"}'
