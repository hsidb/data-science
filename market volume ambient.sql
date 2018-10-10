SELECT Sum(a.TotalAmount) from
    
  (Select  SUM(dwh.corrected_sales_in_value) as TotalAmount
    
FROM
    dex_dwh dwh
JOIN
    nz_coke.causal_shop cs
ON
    dwh.shop_id = cs.shop_id
JOIN
    dex_dpf dpf
ON
    dwh.item_id = dpf.item_id
WHERE
    dpf.characteristic_long_description = 'ITEM LONG DESCRIPTION'
 ) a
