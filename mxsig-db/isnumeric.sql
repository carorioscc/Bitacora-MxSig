-- Function: isnumeric(text)

-- DROP FUNCTION isnumeric(text);

CREATE OR REPLACE FUNCTION isnumeric(text)
  RETURNS boolean AS
$BODY$
SELECT $1 ~ '^[0-9]+$'
$BODY$
  LANGUAGE sql VOLATILE
  COST 100;
ALTER FUNCTION isnumeric(text)
  OWNER TO postgres;
GRANT EXECUTE ON FUNCTION isnumeric(text) TO postgres;
