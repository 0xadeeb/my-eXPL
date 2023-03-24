use crate::ast::*;
use crate::symbol::*;
use crate::type_table::Type;
use crate::utils::{
    err_from_str, label::LabelGenerator, loop_util::LoopStack, register::RegisterPool,
};
use std::collections::LinkedList;
use std::{
    error::Error,
    fs::{File, OpenOptions},
    io::Write,
    path::PathBuf,
};

pub struct CodeGen {
    registers: RegisterPool,
    labels: LabelGenerator,
    lp: LoopStack,
    fd: File,
}

impl CodeGen {
    pub fn new(file_name: &PathBuf) -> Result<CodeGen, Box<dyn Error>> {
        let fd = OpenOptions::new()
            .create(true)
            .truncate(true)
            .write(true)
            .open(file_name)?;
        Ok(CodeGen {
            registers: RegisterPool::default(),
            labels: LabelGenerator::default(),
            lp: LoopStack::default(),
            fd,
        })
    }

    fn pre_call(&mut self) {
        for reg in self.registers.save_context() {
            writeln!(self.fd, "PUSH R{}", reg).unwrap();
        }
    }

    fn post_call(&mut self) {
        for reg in self.registers.restore_context().rev() {
            writeln!(self.fd, "POP R{}", reg).unwrap();
        }
    }

    fn sys_call(&mut self, args: &[&str]) -> Result<u8, Box<dyn Error>> {
        let reg = self
            .registers
            .get_reg()
            .ok_or(err_from_str("No registers left!"))?;
        self.pre_call();
        let ret = self
            .registers
            .get_reg()
            .ok_or(err_from_str("No registers left!"))?;
        for arg in args {
            writeln!(self.fd, "MOV R{}, {}", reg, arg)?;
            writeln!(self.fd, "PUSH R{}", reg)?;
        }
        writeln!(self.fd, "ADD SP, {}", 5 - args.len())?;
        writeln!(self.fd, "CALL 0")?;
        writeln!(self.fd, "POP R{}", ret)?;
        writeln!(self.fd, "SUB SP, 4")?;
        self.post_call();
        self.registers.free_reg(reg);
        Ok(ret)
    }

    fn gen_variable_code(
        &mut self,
        var: &Symbol,
        array_access: &Vec<Tnode>,
        field_access: &Vec<u8>,
    ) -> Result<u8, Box<dyn Error>> {
        let reg1 = self
            .registers
            .get_reg()
            .ok_or(err_from_str("No registers left!"))?;
        match var {
            Symbol::Variable {
                dtype,
                binding,
                is_static,
                ..
            } => match dtype {
                Type::Array { dim, .. } => {
                    writeln!(self.fd, "MOV R{}, {}", reg1, binding)?;
                    for (i, exp) in array_access.iter().enumerate() {
                        let reg2 = self.evaluate(exp)?.unwrap();
                        for d in dim[(i + 1)..].iter() {
                            writeln!(self.fd, "MUL R{}, {}", reg2, d)?;
                        }
                        writeln!(self.fd, "ADD R{}, R{}", reg1, reg2)?;
                        self.registers.free_reg(reg2);
                    }
                }
                _ => {
                    writeln!(self.fd, "MOV R{}, {}", reg1, binding)?;
                    if !is_static {
                        writeln!(self.fd, "ADD R{}, BP", reg1)?;
                    }
                }
            },
            Symbol::Function { .. } => {
                panic!("Symbol of function in var");
            }
        }
        for idx in field_access.iter() {
            writeln!(self.fd, "MOV R{}, [R{}]", reg1, reg1)?;
            writeln!(self.fd, "ADD R{}, {}", reg1, idx)?;
        }
        Ok(reg1)
    }

    fn evaluate(&mut self, node: &Tnode) -> Result<Option<u8>, Box<dyn Error>> {
        match node {
            Tnode::Constant { value, .. } => {
                let reg1 = self
                    .registers
                    .get_reg()
                    .ok_or(err_from_str("No registers left!"))?;
                writeln!(self.fd, "MOV R{}, {}", reg1, value)?;
                Ok(Some(reg1))
            }
            Tnode::Var {
                symbol,
                array_access,
                field_access,
                ref_type,
                ..
            } => {
                let reg1 = self.gen_variable_code(symbol, array_access, field_access)?;
                if let RefType::RHS = ref_type {
                    writeln!(self.fd, "MOV R{}, [R{}]", reg1, reg1)?;
                }
                Ok(Some(reg1))
            }
            Tnode::DeRefOperator { ref_type, var, .. } => {
                let reg1 = self.evaluate(var)?.unwrap();
                if let RefType::RHS = ref_type {
                    writeln!(self.fd, "MOV R{}, [R{}]", reg1, reg1)?;
                }
                Ok(Some(reg1))
            }
            Tnode::RefOperator { var, .. } => {
                let reg1 = self.evaluate(var)?.unwrap();
                Ok(Some(reg1))
            }
            Tnode::BinaryOperator { op, lhs, rhs, .. } => {
                let reg1 = self.evaluate(lhs)?.unwrap();
                let reg2 = self.evaluate(rhs)?.unwrap();
                match op {
                    BinaryOpType::Add => writeln!(self.fd, "ADD R{}, R{}", reg1, reg2)?,
                    BinaryOpType::Sub => writeln!(self.fd, "SUB R{}, R{}", reg1, reg2)?,
                    BinaryOpType::Mul => writeln!(self.fd, "MUL R{}, R{}", reg1, reg2)?,
                    BinaryOpType::Div => writeln!(self.fd, "DIV R{}, R{}", reg1, reg2)?,
                    BinaryOpType::Mod => writeln!(self.fd, "MOD R{}, R{}", reg1, reg2)?,
                    BinaryOpType::EQ => writeln!(self.fd, "EQ R{}, R{}", reg1, reg2)?,
                    BinaryOpType::NE => writeln!(self.fd, "NE R{}, R{}", reg1, reg2)?,
                    BinaryOpType::GT => writeln!(self.fd, "GT R{}, R{}", reg1, reg2)?,
                    BinaryOpType::GE => writeln!(self.fd, "GE R{}, R{}", reg1, reg2)?,
                    BinaryOpType::LT => writeln!(self.fd, "LT R{}, R{}", reg1, reg2)?,
                    BinaryOpType::LE => writeln!(self.fd, "LE R{}, R{}", reg1, reg2)?,
                    BinaryOpType::AND => {
                        writeln!(self.fd, "ADD R{}, R{}", reg1, reg2)?;
                        writeln!(self.fd, "DIV R{}, 2", reg1)?;
                    }
                    BinaryOpType::OR => {
                        writeln!(self.fd, "ADD R{}, R{}", reg1, reg2)?;
                        writeln!(self.fd, "ADD R{}, 1", reg1)?;
                        writeln!(self.fd, "DIV R{}, 2", reg1)?;
                    }
                }
                self.registers.free_reg(reg2);
                Ok(Some(reg1))
            }
            Tnode::Read { var, .. } => {
                let reg1 = self.evaluate(var)?.unwrap();
                let ret = self.sys_call(&["\"Read\"", "-1", &format!("R{}", reg1)])?;
                self.registers.free_reg(reg1);
                self.registers.free_reg(ret);
                Ok(None)
            }
            Tnode::Write { expression, .. } => {
                let reg1 = self.evaluate(expression)?.unwrap();
                let ret = self.sys_call(&["\"Write\"", "-2", &format!("R{}", reg1)])?;
                self.registers.free_reg(reg1);
                self.registers.free_reg(ret);
                Ok(None)
            }
            Tnode::Asgn { lhs, rhs, is_class } => {
                let reg1 = self.evaluate(lhs)?.unwrap();
                let reg2;
                if *is_class {
                    let mut r = rhs.clone();
                    r.set_ref(RefType::LHS).unwrap();
                    reg2 = self.evaluate(&r)?.unwrap();
                    writeln!(self.fd, "MOV [R{}], [R{}]", reg1, reg2)?;
                    writeln!(self.fd, "INR R{}", reg1)?;
                    writeln!(self.fd, "INR R{}", reg2)?;
                    writeln!(self.fd, "MOV [R{}], [R{}]", reg1, reg2)?;
                } else {
                    reg2 = self.evaluate(rhs)?.unwrap();
                    writeln!(self.fd, "MOV [R{}], R{}", reg1, reg2)?;
                }
                self.registers.free_reg(reg2);
                self.registers.free_reg(reg1);
                Ok(None)
            }
            Tnode::Continue => {
                if let Some(label) = self.lp.condition_label() {
                    writeln!(self.fd, "JMP <L{}>", label)?;
                }
                Ok(None)
            }
            Tnode::Break => {
                if let Some(label) = self.lp.exit_label() {
                    writeln!(self.fd, "JMP <L{}>", label)?;
                }
                Ok(None)
            }
            Tnode::If {
                condition,
                if_stmt,
                else_stmt,
                ..
            } => {
                let label1 = self.labels.get();
                let label2 = self.labels.get();
                let reg1 = self.evaluate(condition)?.unwrap();
                writeln!(self.fd, "JZ R{}, <L{}>", reg1, label1)?;
                self.evaluate(if_stmt)?;
                writeln!(self.fd, "JMP <L{}>", label2)?;
                write!(self.fd, "L{}:", label1)?;
                if let Some(stmts) = else_stmt {
                    self.evaluate(stmts)?;
                }
                write!(self.fd, "L{}:", label2)?;
                self.registers.free_reg(reg1);
                Ok(None)
            }
            Tnode::While {
                condition, stmts, ..
            } => {
                let condition_label = self.labels.get();
                let exit_label = self.labels.get();
                self.lp.push((condition_label, exit_label));
                write!(self.fd, "L{}:", condition_label)?;
                let reg1 = self.evaluate(condition)?.unwrap();
                writeln!(self.fd, "JZ R{}, <L{}>", reg1, exit_label)?;
                self.evaluate(stmts)?;
                writeln!(self.fd, "JMP <L{}>", condition_label)?;
                write!(self.fd, "L{}:", exit_label)?;
                self.lp.pop();
                self.registers.free_reg(reg1);
                Ok(None)
            }
            Tnode::Repeat {
                stmts, condition, ..
            } => {
                let stmt_label = self.labels.get();
                let condition_label = self.labels.get();
                let exit_label = self.labels.get();
                self.lp.push((condition_label, exit_label));
                write!(self.fd, "L{}:", stmt_label)?;
                self.evaluate(stmts)?;
                write!(self.fd, "L{}:", condition_label)?;
                let reg1 = self.evaluate(condition)?.unwrap();
                writeln!(self.fd, "JZ R{}, <L{}>", reg1, stmt_label)?;
                write!(self.fd, "L{}:", exit_label)?;
                self.lp.pop();
                Ok(None)
            }
            Tnode::Connector { left, right, .. } => {
                self.evaluate(left)?;
                self.evaluate(right)?;
                Ok(None)
            }
            Tnode::FnCall { symbol, args, .. } => match symbol {
                Symbol::Function { .. } => {
                    self.pre_call();
                    for exp in args.iter() {
                        let reg2 = self.evaluate(exp)?.unwrap();
                        writeln!(self.fd, "PUSH R{}", reg2)?;
                        self.registers.free_reg(reg2);
                    }
                    writeln!(self.fd, "INR SP")?;
                    writeln!(self.fd, "CALL <F{}>", symbol.get_label().unwrap())?;
                    let reg1 = self
                        .registers
                        .get_ret_reg()
                        .ok_or(err_from_str("No registers left!"))?;
                    writeln!(self.fd, "POP R{}", reg1)?;
                    writeln!(self.fd, "SUB SP, {}", args.len())?;
                    self.post_call();
                    Ok(Some(reg1))
                }
                _ => panic!("Function call with symbol of type varaible"),
            },
            Tnode::Return { exp, .. } => {
                let reg1 = self.evaluate(exp)?.unwrap();
                let reg2 = self
                    .registers
                    .get_reg()
                    .ok_or(err_from_str("No registers left!"))?;
                writeln!(self.fd, "MOV R{}, BP", reg2)?;
                writeln!(self.fd, "SUB R{}, 2", reg2)?;
                writeln!(self.fd, "MOV [R{}], R{}", reg2, reg1)?;
                self.registers.free_reg(reg1);
                self.registers.free_reg(reg2);
                Ok(None)
            }
            Tnode::Initialize => {
                let ret = self.sys_call(&["\"Heapset\""])?;
                self.registers.free_reg(ret);
                Ok(None)
            }
            Tnode::Alloc { var, .. } => {
                let reg1 = self.evaluate(var)?.unwrap();
                let reg2 = self.sys_call(&["\"Alloc\"", "8"])?; // TODO: Change 8 to real type size
                writeln!(self.fd, "MOV [R{}], R{}", reg1, reg2)?;
                self.registers.free_reg(reg1);
                self.registers.free_reg(reg2);
                Ok(None)
            }
            Tnode::Free { var, .. } => {
                let reg1 = self.evaluate(var)?.unwrap();
                let reg2 = self.sys_call(&["\"Free\"", &format!("R{}", reg1)])?;
                self.registers.free_reg(reg1);
                self.registers.free_reg(reg2);
                Ok(None)
            }
            Tnode::Null => {
                let reg1 = self
                    .registers
                    .get_reg()
                    .ok_or(err_from_str("No registers left!"))?;
                writeln!(self.fd, "MOV R{}, 0", reg1)?;
                Ok(Some(reg1))
            }
            Tnode::New { c_idx, var, .. } => {
                let reg1 = self.evaluate(var)?.unwrap();
                let reg2 = self.sys_call(&["\"Alloc\"", "8"])?; // TODO: Change 8 to real type size
                writeln!(self.fd, "MOV [R{}], R{}", reg1, reg2)?;
                writeln!(self.fd, "INR R{}", reg1)?;
                writeln!(self.fd, "MOV [R{}], {}", reg1, 4096 + 8 * *c_idx as u16)?;
                self.registers.free_reg(reg1);
                self.registers.free_reg(reg2);
                Ok(None)
            }
            Tnode::Delete { var, .. } => {
                let reg1 = self.evaluate(var)?.unwrap();
                let reg2 = self.sys_call(&["\"Free\"", &format!("R{}", reg1)])?;
                writeln!(self.fd, "INR R{}", reg1)?;
                writeln!(self.fd, "MOV [R{}], -1", reg1)?;
                self.registers.free_reg(reg1);
                self.registers.free_reg(reg2);
                Ok(None)
            }
            Tnode::MethodCall {
                var,
                array_access,
                field_access,
                args,
                symbol,
                ..
            } => {
                let reg1 = self.gen_variable_code(var, array_access, field_access)?;
                let idx = match symbol {
                    Symbol::Function { idx: Some(id), .. } => id,
                    _ => panic!("Method call error"),
                };

                self.pre_call();
                let reg2 = self
                    .registers
                    .get_reg()
                    .ok_or(err_from_str("No registers left!"))?;
                writeln!(self.fd, "MOV R{}, [R{}]", reg2, reg1)?;
                writeln!(self.fd, "PUSH R{}", reg2)?; // Self member field
                writeln!(self.fd, "INR R{}", reg1)?;
                writeln!(self.fd, "MOV R{}, [R{}]", reg1, reg1)?;
                writeln!(self.fd, "PUSH R{}", reg1)?; // Self virt fn table
                self.registers.free_reg(reg2);

                for exp in args.iter() {
                    let reg2 = self.evaluate(exp)?.unwrap();
                    writeln!(self.fd, "PUSH R{}", reg2)?;
                    self.registers.free_reg(reg2);
                }
                writeln!(self.fd, "INR SP")?;
                writeln!(self.fd, "ADD R{}, {}", reg1, idx)?;
                writeln!(self.fd, "MOV R{}, [R{}]", reg1, reg1)?;
                writeln!(self.fd, "CALL R{}", reg1)?;
                let reg2 = self
                    .registers
                    .get_ret_reg()
                    .ok_or(err_from_str("No registers left!"))?;
                writeln!(self.fd, "POP R{}", reg2)?;
                writeln!(self.fd, "SUB SP, {}", args.len() + 2)?;
                self.post_call();
                self.registers.free_reg(reg1);
                Ok(Some(reg2))
            }
            Tnode::Empty => Ok(None),
        }
    }

    fn gen_virt_fn_table(
        &mut self,
        virt_fn_list: &LinkedList<Vec<u8>>,
    ) -> Result<u16, Box<dyn Error>> {
        let mut adr: u16 = 4096;
        for fns in virt_fn_list.iter() {
            let mut counter = adr;
            for fn_idx in fns.iter() {
                writeln!(self.fd, "MOV [{}], <F{}>", counter, fn_idx)?;
                counter += 1;
            }
            adr += 8;
        }
        Ok(adr)
    }

    pub fn emit_code(
        &mut self,
        virt_fn_list: &LinkedList<Vec<u8>>,
        fns: &LinkedList<FnAst>,
        gst_size: u16,
    ) -> Result<(), Box<dyn Error>> {
        write!(
            self.fd,
            "{}\n{}\n{}\n{}\n{}\n{}\n{}\n{}\n",
            0, 2056, 0, 0, 0, 0, 0, 0
        )?;
        let base_adr = self.gen_virt_fn_table(virt_fn_list)?;
        writeln!(self.fd, "MOV SP, {}", base_adr + gst_size)?; // 4095 + 8 * no of classes + gst size + 1 (ret val for main fn)
        writeln!(self.fd, "CALL <F0>")?;
        writeln!(self.fd, "MOV R0, 10")?;
        writeln!(self.fd, "PUSH R0")?;
        writeln!(self.fd, "ADD SP, 4")?;
        writeln!(self.fd, "INT 10")?;
        for fn_def in fns.iter() {
            write!(self.fd, "F{}:", fn_def.get_label())?;
            writeln!(self.fd, "PUSH BP")?;
            writeln!(self.fd, "MOV BP, SP")?;
            writeln!(self.fd, "ADD SP, {}", fn_def.get_lvar_count())?;
            match self.evaluate(fn_def.get_root()) {
                Ok(_) => {
                    writeln!(self.fd, "SUB SP, {}", fn_def.get_lvar_count())?;
                    writeln!(self.fd, "POP BP")?;
                    writeln!(self.fd, "RET")?;
                }
                Err(e) => {
                    eprint!("{e}");
                    return Err(err_from_str("Error in code gen stage!"));
                }
            }
        }
        Ok(())
    }
}
