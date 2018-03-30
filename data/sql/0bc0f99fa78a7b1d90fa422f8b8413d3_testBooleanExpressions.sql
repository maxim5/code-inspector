
-- simple booleans
result := true;
result := false;
result := null;
result := status;

result := var%FOUND;
result := var%ISOPEN;
result := var%NOTFOUND;

result := :var%FOUND;
result := :var%ISOPEN;
result := :var%NOTFOUND;

result := SQL%FOUND;
result := SQL%ISOPEN;
result := SQL%NOTFOUND;

-- composite boleans.
result := a AND b;
result := a OR b;

-- with parens
result := (true);
result := (var);
result := (a AND FALSE);
result := (a OR (b and c));

-- With NOTs
-- simple booleans
result := NOT true;
result := not NOT NoT false;
result := NOT null;
result := NOT status;

result := NOT var%FOUND;
result := NOT var%ISOPEN;
result := NOT var%NOTFOUND;

result := NOT :var%FOUND;
result := NOT :var%ISOPEN;
result := NOT :var%NOTFOUND;

result := NOT SQL%FOUND;
result := NOT SQL%ISOPEN;
result := NOT SQL%NOTFOUND;

-- composite boleans.
result := NOT a AND b;
result := NOT a OR b;

-- with parens
result := NOT (true);
result := NOT (var);
result := NOT (a AND FALSE);
result := NOT (a OR (b and NOT c));

-- is not nulls
result := ('my' || 'string') IS NOT NULL;
result := var + 12 IS NOT NULL;

