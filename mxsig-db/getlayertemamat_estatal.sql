-- Function: getlayertemamat_estatal(geomcoords, character varying, character varying, character varying, numeric, numeric, character varying)

-- DROP FUNCTION getlayertemamat_estatal(geomcoords, character varying, character varying, character varying, numeric, numeric, character varying);

CREATE OR REPLACE FUNCTION getlayertemamat_estatal(
    geomcoords,
    character varying,
    character varying,
    character varying,
    numeric,
    numeric,
    character varying)
  RETURNS SETOF record AS
$BODY$
 declare 
 resultado numeric; 
 r record;
 tablename varchar(40) := trim($2);
 searchsql text := '';
 thewhere temamattype%rowtype;
 colores text;
 count int;
 miwhere varchar := 'true';
 mitabla varchar := ' ce2009.estatal_gransector_view ' ;
 miscampos varchar;
 micampo varchar;
 nuevowhere varchar:='';
 datos varchar[];
 tam int;
 indice int :=1; 
begin
colores := 'SELECT (CASE when ''jksdf'' in (''asfd'') then ''-1''';
count=1;
--raise notice 'voy';
if (isnumeric($4) and $4 <>'0' ) then
 --FOR thewhere IN EXECUTE 'select clausulawhen, case when length(trim(filtro))<2 then '''' else ''and '' || filtro end as filtro, tabla from temas.temamat where id = ' || $4 || ';'
 FOR thewhere IN EXECUTE 'select clausulawhen, case when length(trim(filtro))<2 then '''' else ''    '' || filtro end as filtro, tabla ,variable from temas.temamat where id = ' || $4 || ';'
 loop
	-- colores := thewhere.cuantos;
	--raise notice 'datos ---%', thewhere;
	LOOP
		 --some computations
		-- raise notice 'aki %', thewhere.clausulawhen;
		 colores := colores || '  ' ||  thewhere.clausulawhen;
		 --micampo := substring(thewhere.clausulawhen from 7 for (position('>' in thewhere.clausulawhen)-7));
		 micampo := 'cvegeo';
		  raise notice 'CAMPO %', micampo;
		 miwhere := thewhere.filtro;
		 mitabla := thewhere.tabla;
		 EXIT;
		  --raise notice '---- %', thewhere.claves[count];
		  --raise notice '%', thewhere.colores[count];
	END LOOP;
	--raise notice '%', colores;
 end loop;
--end if;
colores := colores || ' when  '||thewhere.variable||' = ''-8'' then ''-8'' else ''-9'' end)::varchar as color, ';  
 if($1.x2 > $1.x1) then 
  resultado := abs($1.x2 -$1.x1);
  --raise notice 'x2 > x1';
 end if;
 if($1.x1 > $1.x2) then 
  resultado := abs($1.x1 -$1.x2);
  --raise notice 'x1 > x2';
 end if;

--raise notice 'ejecutando miwhere %' , miwhere;

 --raise notice 'resultado = % x1 = % x2 = %', resultado, $3, $4;

  if(resultado > 0) then
  -- raise notice 'resultado esta dentro de los rangos';
if (miwhere<>'true') then
  miwhere := substring(miwhere,4,length(miwhere));
 datos := regexp_split_to_array(lower(miwhere),'and');
 tam :=  cast((regexp_split_to_array(replace(array_dims(datos),']',''),':'))[2] as int);
 --raise notice 'tam %' , tam;
 /*if (tam>1) then
     LOOP
	if (datos[indice] like '%'||mitabla||'%') then 
		if (length(nuevowhere) > 0) then
		    nuevowhere := nuevowhere || ' and ';
		end if;	
		nuevowhere := nuevowhere || ' ' || replace(datos[indice], mitabla, $2);
	end if;
	indice:=indice+1;
	if (indice > tam) then
		exit;
	end if;
    END LOOP;
  end if;
  if (tam=1) then
	if (datos[1] like '%'||mitabla||'%') then 
	   raise notice 'datos %' , datos[1];
	    nuevowhere := replace(datos[1], mitabla, $2);
	end if;	
  end if; */
  if (nuevowhere is null) then
	nuevowhere:='';
  end if;
  if (length(nuevowhere)>0) then
	nuevowhere := ' and ' || nuevowhere;
  end if;
  if (miwhere is null ) then
	miwhere:='';
  end if;
  if (length(miwhere)>0) then
	miwhere := ' where ' || miwhere;
  end if;
  resultado:=6;
      
	searchsql:= colores || $7 || ', ' || $2 || '.gid, the_geom from ' || $2 || ' left join ( select '||mitabla||'.cvegeo, '||thewhere.variable ||' from ' || mitabla || miwhere || ') b on ' || $2 || '.cvegeo = b.cvegeo where GeomFromText(''' || $1.the_geometry  || ''', 900913) && the_geom ' || nuevowhere; 
	--searchsql:= colores || $7 || ', ' || $2 || '.gid, the_geom from ' || $2 || ' left join ' || mitabla || ' on ' || mitabla || '.cvegeo = '|| $2 || '.cvegeo where GeomFromText(''' || $1.the_geometry  || ''', 4326) && the_geom ' || miwhere; 
	--searchsql := colores || $7 || ', ' || $2 || '.gid, the_geom from ' || $2 || ' left join ' || mitabla || ' on ' || mitabla | |'.cvegeo = '|| $2 || '.cvegeo where GeomFromText(''' || $1.the_geometry  || ''', 4326) && the_geom and ' || miwhere;
	--raise notice 'sql %' , searchsql;
  
end if;
if (miwhere='true') then
  searchsql := colores || $7 || ', ' || $2 || '.gid, the_geom from ' || $2 || ' limit 0 ';
end if;
  --raise notice 'ejecutando %' , searchsql;
  FOR r IN EXECUTE(searchsql)
  LOOP
        -- can do some processing here
        RETURN NEXT r; -- return current row of SELECT
    END LOOP;
    RETURN;
    else
	raise notice 'resultado no esta en rango';
	
  end if;
end if;
 RETURN;
end;
$BODY$
  LANGUAGE plpgsql IMMUTABLE
  COST 100
  ROWS 1000;
ALTER FUNCTION getlayertemamat_estatal(geomcoords, character varying, character varying, character varying, numeric, numeric, character varying)
  OWNER TO postgres;
GRANT EXECUTE ON FUNCTION getlayertemamat_estatal(geomcoords, character varying, character varying, character varying, numeric, numeric, character varying) TO public;
