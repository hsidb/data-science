SELECT
    dwh.item_id,
    dpf.characteristic_value_long_description product,
    SUM(dwh.corrected_sales_in_value) sales_value,
    sum(dwh.corrected_sales_in_value)/4.72E8 percentage
    
FROM
    dex_dwh dwh
JOIN
    nz_ambient.causal_shop cs
ON
    dwh.shop_id = cs.shop_id
JOIN
    dex_dpf dpf
ON
    dwh.item_id = dpf.item_id
WHERE
    dpf.characteristic_long_description = 'ITEM LONG DESCRIPTION'
GROUP BY
    1,
    2
    
ORDER BY
    4 DESC