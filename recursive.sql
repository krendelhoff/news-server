with result as (with recursive cats as (select category,subcategory from categories_content where category='0d1f155e-913d-4ccd-a654-a9d2a6a38466' union select ct.category,ct.subcategory from categories_content ct inner join cats on cats.subcategory=ct.category) select * from cats) select category from result union select subcategory from result;