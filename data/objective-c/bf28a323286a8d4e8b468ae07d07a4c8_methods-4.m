@implementation Foo (Bar)
@end

@implementation Foo <X, Y, Z>
@end

@implementation Foo : Bar <X, Y, Z>

- init
{
    return [self initWithCString:""];
}

- (int(*)(elt,elt)) comparisonFunction
{
    return elt_compare_chars;
}

- (t1) gazonk: (t2) u, t3 x, int y, ...
{
}

@end

// Local Variables:
// font-lock-maximum-decoration: 2
// End:
