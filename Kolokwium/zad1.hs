--helper_change powsatał jako pierwszy, gdy jeszcze nie zauważyłem że indeksy w ps mogą się powtarzać, na losowych pozycjach
--jako, że nie za bardzo miałem czas, to po prostu zastosowałem funkcję która iteruję po ps-ie i zamienia tylko jeden element listy na raz.
--Karmię ją kolejnymi ps-ami, czyli wywołuję ją z jednym argumentem ps, następnie wywołuje ją znowu z kolejnym,
--już na zmienionej liście:
--change [1,1,1,1,1,1] [(0,100), (4,200), (0,300)] -> [100,1,..] -> [100,1,1,1,200,1] -> [300, 1, 1, 1, 200, 1]
--Obrzydliwie wręcz niewydajne, ale taka jest konsekwencja nie czytania wszystkich przykładów. "Ważne że działa"
helper_change _ _ [] _ _ = []
helper_change count res (x:xs) (y1,y2) ps = if count == y1 then res++[y2]++(helper_change (count + 1) res xs (head ps) (tail ps))
                                        else res++[x]++(helper_change (count + 1) res xs (y1,y2) ps)

ps_iterator res _ [] = res
ps_iterator res list (p:ps) = ps_iterator tmp_res tmp_res ps where tmp_res = (helper_change 0 [] list p [p])  


change list ps = ps_iterator [] list ps



 