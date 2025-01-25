use ast::{Block, Expression, Ident, Literal, Statement};
use miette::Result;

pub struct Interpreter<'src, 'i> {
    outer_scope: Scope<'src, 'i>,
}

impl<'src> Interpreter<'src, '_> {
    pub fn new() -> Self {
        Self { outer_scope: Scope::default() }
    }

    pub fn run_file(&mut self, file: ast::File<'src>) -> Result<()> {
        for item in file.items {
            match item {
                ast::Item::Fn(f) => {
                    self.outer_scope.functions.push(f);
                }
            }
        }

        self.run_main_file()?;

        Ok(())
    }

    fn run_main_file(&self) -> Result<()> {
        let main_fn = self.outer_scope.get_function_by_name("main")?;
        self.run_fn(main_fn)?;

        Ok(())
    }

    fn run_fn(&self, f: &ast::Fn<'src>) -> Result<()> {
        let mut scope = Scope::default();
        scope.parent = Some(&self.outer_scope);

        // FIXME: Add function arguments to the scope
        let args = vec!["test arg"];

        if f.name == Ident("print") {
            intrinsics::print(Expression::Literal(Literal::Str(args[0])));
            return Ok(());
        }

        self.run_block(&f.body, &mut scope)?;

        Ok(())
    }

    fn run_block(&self, block: &Block<'src>, scope: &mut Scope<'src, '_>) -> Result<()> {
        for statement in &block.statements {
            self.run_statement(statement, scope)?;
        }

        Ok(())
    }

    fn run_statement(
        &self,
        statement: &Statement<'src>,
        scope: &mut Scope<'src, '_>,
    ) -> Result<()> {
        match statement {
            Statement::Expression(expression) => {
                self.run_expression(expression, scope)?;
            }
        }

        Ok(())
    }

    fn run_expression(
        &self,
        expression: &Expression<'src>,
        scope: &mut Scope<'src, '_>,
    ) -> Result<()> {
        match expression {
            Expression::FnCall { callee, args: _ } => {
                let callee = match &**callee {
                    Expression::Ident(ident) => ident.0,
                    _ => todo!(),
                };

                let function = scope.get_function_by_name(callee)?;
                self.run_fn(function)?;
            }
            _ => todo!(),
        }

        Ok(())
    }
}

pub struct Scope<'src, 'i> {
    pub functions: Vec<ast::Fn<'src>>,

    pub parent: Option<&'i Scope<'src, 'i>>,
}

impl Default for Scope<'_, '_> {
    fn default() -> Self {
        Self { functions: intrinsics::INTRINSIC_FNS.to_vec(), parent: None }
    }
}

impl Scope<'_, '_> {
    pub fn get_function_by_name(&self, name: &str) -> Result<&ast::Fn> {
        match self.functions.iter().find(|f| f.name == Ident(name)) {
            Some(f) => return Ok(f),
            None => {
                if let Some(parent) = self.parent {
                    return parent.get_function_by_name(name);
                }

                miette::bail!("function '{}' not found", name);
            }
        }
    }
}

mod intrinsics {
    use ast::{Block, Expression, Ident, Literal};

    pub const PRINT_FN: ast::Fn<'static> =
        ast::Fn { name: Ident("print"), body: Block { statements: vec![] }, params: () };

    pub const INTRINSIC_FNS: &[ast::Fn<'static>] = &[PRINT_FN];

    pub fn print(msg: Expression) {
        match msg {
            Expression::Literal(Literal::Str(s)) => {
                println!("{}", s);
            }
            _ => todo!(),
        }
    }
}
