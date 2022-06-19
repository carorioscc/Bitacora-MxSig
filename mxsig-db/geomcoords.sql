-- Type: geomcoords

-- DROP TYPE geomcoords;

CREATE TYPE geomcoords AS
   (x1 numeric,
    x2 numeric,
    the_geometry text);
ALTER TYPE geomcoords
  OWNER TO postgres;
