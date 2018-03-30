//
//  Rule.m
//  Bloque
//
//  Created by Woo-Kyoung Noh on 12/03/10.
//  Copyright 2010 factorcat. All rights reserved.
//

#import "Rule.h"
#import "NSStringExt.h"
#import "NSStringEBNF.h"
#import "Block.h"

id (^ full_stop )() = ^() {
	return [Rule ruleWithSymbol:DOT];
};

id (^ ampersand )(NSString* instance) = ^(NSString* instance) {
	return [Rule ruleWithSymbol:AMP instance:instance];
};

id (^ exclamation_mark )(NSString* instance) = ^(NSString* instance) {
	return [Rule ruleWithSymbol:EXCLAMATION_MARK instance:instance];
};

id (^ square_bracket )(NSString* instance) = ^(NSString* instance) {
	return [Rule ruleWithSymbol:SQUARE_BRACKET instance:instance];
};



@implementation Rule
@synthesize symbol;
@synthesize instance;
@synthesize action;

+ (id) ruleWithSymbol:(NSString*)symbol_ {
	Rule* rule = [[[Rule alloc] init] autorelease];
	rule.symbol = symbol_;
	rule.action = nil;
	return rule;
}

+ (id) ruleWithSymbol:(NSString*)symbol_ instance:(NSString*)instance_ {
	Rule* rule = [self ruleWithSymbol:symbol_];
	rule.instance = instance_;
	return rule;
}

- (void)dealloc {
	[self.symbol release];
	[self.instance release];
	[self.action release];
	[super dealloc];
}

-(NSString*) sub_symbol {
	if ([self.instance isKindOfClass:[NSArray class]]) {
		Rule* subRule = [Rule fromElement:[self.instance lastObject]];
		return subRule.symbol;		
	} else {
		return nil;		
	}
}

-(NSCharacterSet*) to_charset {
	if ([EXCLAMATION_MARK isEqualToString:self.symbol]) {
		return [[NSCharacterSet characterSetWithCharactersInString:self.instance] invertedSet];
	} else if ([DOT isEqualToString:self.symbol]) {
		return [[NSCharacterSet nonBaseCharacterSet] invertedSet];
	} else if ([STAR isEqualToString:self.symbol]) {
		if ([self.instance isKindOfClass:[NSArray class]]) {
			NSMutableCharacterSet* charSet = [NSMutableCharacterSet nonBaseCharacterSet];
			NSArray* ary = (NSArray*)self.instance;
			int aryCount = ary.count;
			if (aryCount > 0) {
				id firstElement = [ary objectAtIndex:0];
				Rule* firstRule = [Rule fromElement:firstElement];
				charSet = [[[firstRule to_charset] mutableCopy] autorelease];
			}
			if (aryCount > 1) {
				int idx;
				for (idx = 1; idx < aryCount; idx++) {
					id element = [ary objectAtIndex:idx];
					Rule* rule = [Rule fromElement:element];
					[charSet formIntersectionWithCharacterSet:[rule to_charset]];
				}
			}
			return charSet;
		} else {
			return [NSCharacterSet characterSetWithCharactersInString:self.instance];
		}
	} else if ([PIPE isEqualToString:self.symbol]) {			
		NSMutableCharacterSet* tokensSet = [NSMutableCharacterSet nonBaseCharacterSet];
		for (NSString* ch in self.instance) {
			[tokensSet addCharactersInString:ch];
		}
		return tokensSet;
	} else if ([QUESTION_MARK isEqualToString:self.symbol]) {
		return [NSCharacterSet characterSetWithCharactersInString:self.instance];
	} else if ([SQUARE_BRACKET isEqualToString:self.symbol]) {
		return [self range_of_chars_to_charset:self.instance];
	} else {
		return [NSCharacterSet nonBaseCharacterSet];
	}
}

-(NSCharacterSet*) range_of_chars_to_charset:(NSString*)str {
	NSMutableCharacterSet* charset = [NSMutableCharacterSet nonBaseCharacterSet];
	int idx = 0;
	while (true) {
		unichar ch = [str characterAtIndex:idx];
		if (CHAR_MINUS == ch) {
			int prevIdx = idx - 1;
			int nextIdx = idx + 1;
			unichar prevChar = [str characterAtIndex:prevIdx];
			unichar nextChar = [str characterAtIndex:nextIdx];
			NSRange range = NSMakeRange(prevChar + 1, nextChar - (prevChar + 1));
			[charset addCharactersInRange:range];
			idx += 2;
		} else {
			[charset addCharactersInString:[NSString stringWithCharacter:ch]];
			idx += 1;			
		}
		if (idx >= str.length) {
			break;
		}
	}
	return charset;
}

+(Rule*) fromElement:(id)element {
	Rule* rule;
	if ([element isKindOfClass:[PLBlock class]]) {
		id (^ block )() = element;
		rule = block();
	} else {
		rule = element;
	}
	return rule;
}

-(id) action:(Quotation*)quot {
	self.action = quot;
	return self;
}

@end



@implementation Rule (EBNF)
-(Rule*) pipe:(NSString*)instance_ {
	[self.instance addObject:instance_];
	return self;
}
@end