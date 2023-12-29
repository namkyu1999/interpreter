package parser

import (
	"fmt"
	"interpreter_go/ast"
	"interpreter_go/lexer"
	"interpreter_go/token"
	"strconv"
)

var precedences = map[token.TokenType]int{
	token.Equal:       Equals,
	token.NotEqual:    Equals,
	token.LessThan:    LessGreater,
	token.GreaterThan: LessGreater,
	token.Plus:        Sum,
	token.Minus:       Sum,
	token.Slash:       Product,
	token.Asterisk:    Product,
	token.LeftBracket: Index,
}

func (p *Parser) peekPrecedence() int {
	// Get the precedence of the peek token
	if p, ok := precedences[p.peekToken.Type]; ok {
		return p
	}
	return Lowest
}

func (p *Parser) curPrecedence() int {
	// Get the precedence of the current token
	if p, ok := precedences[p.curToken.Type]; ok {
		return p
	}
	return Lowest
}

const (
	_ int = iota
	Lowest
	Equals      // ==
	LessGreater // > or <
	Sum         // +
	Product     // *
	Prefix      // -X or !X
	Call        // myFunction(X)
	Index       // array[index]
)

type (
	prefixParseFn func() ast.Expression
	infixParseFn  func(ast.Expression) ast.Expression
)

type Parser struct {
	l         *lexer.Lexer
	curToken  token.Token
	peekToken token.Token
	errors    []string

	prefixParseFns map[token.TokenType]prefixParseFn
	infixParseFns  map[token.TokenType]infixParseFn
}

func New(l *lexer.Lexer) *Parser {
	// Create a new parser that holds a lexer
	p := &Parser{
		l:      l,
		errors: []string{},
	}

	// Read two tokens, so curToken and peekToken are both set
	p.nextToken()
	p.nextToken()

	// Register prefix parse functions
	p.prefixParseFns = make(map[token.TokenType]prefixParseFn)
	p.registerPrefix(token.Identifier, p.parseIdentifier)
	p.registerPrefix(token.Int, p.parseIntegerLiteral)
	p.registerPrefix(token.Bang, p.parsePrefixExpression)
	p.registerPrefix(token.Minus, p.parsePrefixExpression)
	p.registerPrefix(token.True, p.parseBoolean)
	p.registerPrefix(token.False, p.parseBoolean)
	p.registerPrefix(token.LeftParen, p.parseGroupedExpression)
	p.registerPrefix(token.If, p.parseIfExpression)
	p.registerPrefix(token.Function, p.parseFunctionLiteral)
	p.registerPrefix(token.String, p.parseStringLiteral)
	p.registerPrefix(token.LeftBracket, p.parseArrayLiteral)

	p.infixParseFns = make(map[token.TokenType]infixParseFn)
	p.registerInfix(token.Equal, p.parseInfixExpression)
	p.registerInfix(token.NotEqual, p.parseInfixExpression)
	p.registerInfix(token.LessThan, p.parseInfixExpression)
	p.registerInfix(token.GreaterThan, p.parseInfixExpression)
	p.registerInfix(token.Plus, p.parseInfixExpression)
	p.registerInfix(token.Minus, p.parseInfixExpression)
	p.registerInfix(token.Slash, p.parseInfixExpression)
	p.registerInfix(token.Asterisk, p.parseInfixExpression)
	p.registerInfix(token.LeftParen, p.parseCallExpression)
	p.registerInfix(token.LeftBracket, p.parseIndexExpression)
	return p
}

func (p *Parser) parseCallExpression(function ast.Expression) ast.Expression {
	// Create the AST node
	exp := &ast.CallExpression{Token: p.curToken, Function: function}
	exp.Arguments = p.parseExpressionList(token.RightParen)
	return exp
}

func (p *Parser) parseCallArguments() []ast.Expression {
	// Create the AST node
	args := []ast.Expression{}

	// If the next token is a right parenthesis, return the arguments
	if p.peekTokenIs(token.RightParen) {
		// Advance to the next token
		p.nextToken()
		return args
	}

	// Advance to the next token
	p.nextToken()
	args = append(args, p.parseExpression(Lowest))

	for p.peekTokenIs(token.Comma) {
		// Advance to the next token
		p.nextToken()
		// Advance to the next token
		p.nextToken()
		args = append(args, p.parseExpression(Lowest))
	}

	// Expect a right parenthesis
	if !p.expectPeek(token.RightParen) {
		return nil
	}

	return args
}

func (p *Parser) parseFunctionLiteral() ast.Expression {
	// Create the AST node
	lit := &ast.FunctionLiteral{Token: p.curToken}

	// Expect a left parenthesis
	if !p.expectPeek(token.LeftParen) {
		return nil
	}

	// Parse the parameters
	lit.Parameters = p.parseFunctionParameters()

	// Expect a left brace
	if !p.expectPeek(token.LeftBrace) {
		return nil
	}

	// Parse the body
	lit.Body = p.parseBlockStatement()

	return lit
}

func (p *Parser) parseFunctionParameters() []*ast.Identifier {
	// Create the AST node
	identifiers := []*ast.Identifier{}

	// If the next token is a right parenthesis, return the identifiers
	if p.peekTokenIs(token.RightParen) {
		// Advance to the next token
		p.nextToken()
		return identifiers
	}

	// Advance to the next token
	p.nextToken()

	// Create the first identifier
	ident := &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal}
	identifiers = append(identifiers, ident)

	for p.peekTokenIs(token.Comma) {
		// Advance to the next token
		p.nextToken()
		// Advance to the next token
		p.nextToken()

		// Create the identifier
		ident := &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal}
		identifiers = append(identifiers, ident)
	}

	// Expect a right parenthesis
	if !p.expectPeek(token.RightParen) {
		return nil
	}

	return identifiers
}

func (p *Parser) parseIfExpression() ast.Expression {
	// Create the AST node
	expression := &ast.IfExpression{Token: p.curToken}

	// Expect a left parenthesis
	if !p.expectPeek(token.LeftParen) {
		return nil
	}

	// Advance to the next token
	p.nextToken()

	// Parse the condition
	expression.Condition = p.parseExpression(Lowest)

	// Expect a right parenthesis
	if !p.expectPeek(token.RightParen) {
		return nil
	}

	// Expect a left brace
	if !p.expectPeek(token.LeftBrace) {
		return nil
	}

	// Parse the consequence
	expression.Consequence = p.parseBlockStatement()

	if p.peekTokenIs(token.Else) {
		// Advance to the next token
		p.nextToken()

		// Expect a left brace
		if !p.expectPeek(token.LeftBrace) {
			return nil
		}

		// Parse the consequence
		expression.Alternative = p.parseBlockStatement()
	}

	return expression
}

func (p *Parser) parseBlockStatement() *ast.BlockStatement {
	// Create the AST node
	block := &ast.BlockStatement{Token: p.curToken}
	block.Statements = []ast.Statement{}

	// Advance to the next token
	p.nextToken()

	// Loop until the next token is not a right brace
	for !p.curTokenIs(token.RightBrace) {
		// Parse a statement
		stmt := p.parseStatement()
		if stmt != nil {
			block.Statements = append(block.Statements, stmt)

			// Advance to the next token
			p.nextToken()
		}
	}

	return block
}

func (p *Parser) parseGroupedExpression() ast.Expression {
	// Advance to the next token
	p.nextToken()

	// Parse the expression
	exp := p.parseExpression(Lowest)

	// Expect a closing parenthesis
	if !p.expectPeek(token.RightParen) {
		return nil
	}

	return exp
}

func (p *Parser) parseIdentifier() ast.Expression {
	// Create the AST node
	return &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal}
}

func (p *Parser) parsePrefixExpression() ast.Expression {
	// Create the AST node
	expression := &ast.PrefixExpression{
		Token:    p.curToken,
		Operator: p.curToken.Literal,
	}

	p.nextToken()

	expression.Right = p.parseExpression(Prefix)

	return expression
}

func (p *Parser) parseBoolean() ast.Expression {
	// Create the AST node
	return &ast.Boolean{Token: p.curToken, Value: p.curTokenIs(token.True)}
}

func (p *Parser) parseInfixExpression(left ast.Expression) ast.Expression {
	// Create the AST node
	expression := &ast.InfixExpression{
		Token:    p.curToken,
		Operator: p.curToken.Literal,
		Left:     left,
	}

	// Get the precedence of the current token
	precedence := p.curPrecedence()

	// Advance to the next token
	p.nextToken()

	// Parse the right expression
	expression.Right = p.parseExpression(precedence)

	return expression
}

func (p *Parser) Errors() []string {
	// Return the parser errors
	return p.errors
}

func (p *Parser) peekError(t token.TokenType) {
	// Add an error to the parser errors
	msg := fmt.Sprintf("expected next token to be %s, got %s instead", t, p.peekToken.Type)
	p.errors = append(p.errors, msg)
}

func (p *Parser) nextToken() {
	// Advance both curToken and peekToken
	p.curToken = p.peekToken
	p.peekToken = p.l.NextToken()
}

func (p *Parser) ParseProgram() *ast.Program {
	program := &ast.Program{}
	program.Statements = []ast.Statement{}
	for p.curToken.Type != token.EOF {
		// Parse a statement
		stmt := p.parseStatement()
		if stmt != nil {
			program.Statements = append(program.Statements, stmt)
		}
		// Advance to the next token
		p.nextToken()
	}
	return program
}

func (p *Parser) parseStatement() ast.Statement {
	switch p.curToken.Type {
	case token.Let:
		// Parse a let statement
		return p.parseLetStatement()
	case token.Return:
		// Parse a return statement
		return p.parseReturnStatement()
	default:
		return p.parseExpressionStatement()
	}
}

func (p *Parser) parseLetStatement() *ast.LetStatement {
	// Create the AST node
	stmt := &ast.LetStatement{Token: p.curToken}

	// Expect an identifier
	if !p.expectPeek(token.Identifier) {
		return nil
	}

	// Set the identifier
	stmt.Name = &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal}

	// Expect an equal sign
	if !p.expectPeek(token.Assign) {
		return nil
	}

	p.nextToken()

	// Parse the expression
	stmt.Value = p.parseExpression(Lowest)

	// Expect a semicolon
	if p.peekTokenIs(token.Semicolon) {
		// Advance to the next token
		p.nextToken()
	}

	return stmt
}

func (p *Parser) parseReturnStatement() *ast.ReturnStatement {
	// Create the AST node
	stmt := &ast.ReturnStatement{Token: p.curToken}

	// Advance to the next token
	p.nextToken()

	// Parse the expression
	stmt.ReturnValue = p.parseExpression(Lowest)

	for !p.curTokenIs(token.Semicolon) {
		p.nextToken()
	}

	return stmt
}

func (p *Parser) curTokenIs(t token.TokenType) bool {
	// Check if the current token is of the given type
	return p.curToken.Type == t
}

func (p *Parser) peekTokenIs(t token.TokenType) bool {
	// Check if the peek token is of the given type
	return p.peekToken.Type == t
}

func (p *Parser) expectPeek(t token.TokenType) bool {
	// Check if the peek token is of the given type
	if p.peekTokenIs(t) {
		// If it is, advance to the next token
		p.nextToken()
		return true
	} else {
		// Otherwise, return false
		p.peekError(t)
		return false
	}
}

func (p *Parser) registerPrefix(tokenType token.TokenType, fn prefixParseFn) {
	// Register a prefix parse function
	p.prefixParseFns[tokenType] = fn
}

func (p *Parser) registerInfix(tokenType token.TokenType, fn infixParseFn) {
	// Register an infix parse function
	p.infixParseFns[tokenType] = fn
}

func (p *Parser) parseExpressionStatement() *ast.ExpressionStatement {
	// Create the AST node
	stmt := &ast.ExpressionStatement{Token: p.curToken}

	stmt.Expression = p.parseExpression(Lowest)

	if p.peekTokenIs(token.Semicolon) {
		// Advance to the next token
		p.nextToken()
	}

	return stmt
}

func (p *Parser) parseExpression(precedence int) ast.Expression {
	// Parse an expression
	prefix := p.prefixParseFns[p.curToken.Type]
	if prefix == nil {
		p.noPrefixParseFnError(p.curToken.Type)
		return nil
	}

	// Call the prefix parse function
	leftExp := prefix()

	// Loop until the next token is not a semicolon and the precedence is not lower than the next token's precedence
	for !p.peekTokenIs(token.Semicolon) && precedence < p.peekPrecedence() {
		// Get the infix parse function
		infix := p.infixParseFns[p.peekToken.Type]
		if infix == nil {
			// Return the left expression
			return leftExp
		}

		// Advance to the next token
		p.nextToken()

		// Call the infix parse function
		leftExp = infix(leftExp)
	}

	return leftExp
}

func (p *Parser) parseIntegerLiteral() ast.Expression {
	lit := &ast.IntegerLiteral{Token: p.curToken}

	value, err := strconv.ParseInt(p.curToken.Literal, 0, 64)
	if err != nil {
		// Error parsing the integer
		msg := fmt.Sprintf("could not parse %q as integer", p.curToken.Literal)
		p.errors = append(p.errors, msg)
		return nil
	}

	lit.Value = value
	return lit
}

func (p *Parser) noPrefixParseFnError(t token.TokenType) {
	// Add an error to the parser errors
	msg := fmt.Sprintf("no prefix parse function for %s found", t)
	p.errors = append(p.errors, msg)
}

func (p *Parser) parseStringLiteral() ast.Expression {
	// Create the AST node
	return &ast.StringLiteral{Token: p.curToken, Value: p.curToken.Literal}
}

func (p *Parser) parseArrayLiteral() ast.Expression {
	// Create the AST node
	array := &ast.ArrayLiteral{Token: p.curToken}

	// Parse the elements
	array.Elements = p.parseExpressionList(token.RightBracket)

	return array
}

func (p *Parser) parseExpressionList(end token.TokenType) []ast.Expression {
	// Create the AST node
	list := []ast.Expression{}

	// If the next token is the end token, return the list
	if p.peekTokenIs(end) {
		// Advance to the next token
		p.nextToken()
		return list
	}

	// Advance to the next token
	p.nextToken()

	// Parse the first expression
	list = append(list, p.parseExpression(Lowest))

	for p.peekTokenIs(token.Comma) {
		// Advance to the next token
		p.nextToken()
		// Advance to the next token
		p.nextToken()
		list = append(list, p.parseExpression(Lowest))
	}

	// Expect the end token
	if !p.expectPeek(end) {
		return nil
	}

	return list
}

func (p *Parser) parseIndexExpression(left ast.Expression) ast.Expression {
	// Create the AST node
	exp := &ast.IndexExpression{Token: p.curToken, Left: left}

	// Advance to the next token
	p.nextToken()

	// Parse the index
	exp.Index = p.parseExpression(Lowest)

	// Expect a right bracket
	if !p.expectPeek(token.RightBracket) {
		return nil
	}

	return exp
}
