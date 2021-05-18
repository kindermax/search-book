use std::collections::HashMap;
use std::collections::HashSet;

#[derive(Debug)]
struct InvertedIndex {
    index: HashMap<String, Vec<usize>>,
    curr_doc_id: usize,
}


#[derive(Debug, PartialEq)]
enum Token {
    Or,
    And,
    OpenPar,
    ClosePar,
    Word(String),
}

fn tokenize(raw: &str) -> Result<Vec<Token>, String> {
    let mut tokens: Vec<Token> = vec![];
        let mut buf = String::new();

        for ch in raw.chars() {
            match ch {
                '(' => {
                    tokens.push(Token::OpenPar)
                },
                ')' => {
                    tokens.push(Token::Word(buf.clone()));
                    buf.clear();

                    tokens.push(Token::ClosePar)
                },
                'a'..='z' | 'A'..='Z' => {
                    buf.push(ch)
                },
                ch if ch.is_whitespace() => {
                    if buf.is_empty() {
                        continue;
                    }
                    let token = match buf.to_lowercase().as_str() {
                        "or" => Token::Or,
                        "and" => Token::And,
                        val => Token::Word(val.to_string())
                    };
                    tokens.push(token);
                    buf.clear()
                },
                _ => {
                    unreachable!("unexpected char");
                }
            }
        }

        tokens.push(Token::Word(buf.clone()));
        buf.clear();

        Ok(tokens)

}


#[derive(Debug, PartialEq)]
enum AstNode {
    Word(String),
    And { nodes: Vec<AstNode>, },
    Or { nodes: Vec<AstNode>, }
}

impl AstNode {
    fn or(nodes: Vec<AstNode>) -> AstNode {
        AstNode::Or { nodes }
    }

    fn and(nodes: Vec<AstNode>) -> AstNode {
        AstNode::And { nodes }
    }
}

#[derive(Debug, PartialEq)]
struct Query { // TODO rename to ast
    pub ast: AstNode,
}

impl Query {
    fn new(raw_query: &str) -> Result<Query, String> {
        let tokens = tokenize(raw_query)?;

        fn make_node(stack_ops: &mut Vec<Token>, stack_ast: &mut Vec<AstNode>) -> Result<AstNode, String> {
            let left = stack_ast.pop().unwrap();
            let right = stack_ast.pop().unwrap();
            let node = match stack_ops.pop().unwrap() {
                Token::Or => {
                    AstNode::or(vec![left, right])
                },
                Token::And => {
                    AstNode::and(vec![left, right])
                }
                _ => unreachable!()
            };
            stack_ops.pop();

            Ok(node)
        }

        let mut stack_ops = Vec::<Token>::new();
        let mut stack_ast = Vec::<AstNode>::new();

        for token in tokens {
            match token {
                Token::OpenPar | Token::Or | Token::And => {
                    stack_ops.push(token);
                }
                Token::ClosePar => {
                    let node  = make_node(&mut stack_ops, &mut stack_ast)?;
                    stack_ast.push(node);
                }
                Token::Word(word) => {
                    stack_ast.push(AstNode::Word(word));
                }
            }
        }

        let node  = make_node(&mut stack_ops, &mut stack_ast)?;

        Ok(Query { ast: node })
    }
}


impl InvertedIndex {
    fn new() -> Self {
        Self {
            index: HashMap::new(),
            curr_doc_id: 0,
        }
    }

    /// Adding(indexing) a document to an index.
    fn add_doc(&mut self, doc: String) {
        let next_doc_id = self.curr_doc_id;
        for word in doc.split(" ") {
            self.index
                .entry(word.to_string())
                .and_modify(|store| store.push(next_doc_id))
                .or_insert(vec![next_doc_id]);
        }
        self.curr_doc_id += 1;
    }

    fn words(&self) -> usize {
        self.index.keys().len()
    }

    fn search_word(&self, word: &str) -> Option<Vec<usize>> {
        self.index.get(word).and_then(|val| Some(val.clone()))
    }

    fn search(&self, query: Query) -> Result<Vec<usize>, String> {
        let result = self.index
            .iter()
            .filter_map(|(key, docs)| {
                // if query.conj.contains(key) {
                //     Some(HashSet::from_iter(docs.iter().cloned()))
                // } else {
                //     None
                // }
                None
            })
            .fold(HashSet::new(), |acc, item| {
                if acc.is_empty() {
                    item
                } else {
                    acc.intersection(&item).copied().collect()
                }
            })
            .into_iter()
            .collect();

        Ok(result)

    }
}

fn main() {
    let mut index = InvertedIndex::new();
    index.add_doc("new home sales to forecasts".to_string());
    index.add_doc("home sales rise in july".to_string());
    index.add_doc("increase in home sales in july".to_string());
    index.add_doc("july new home sales rise".to_string());
    println!("{:?}", index);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_build_index() {
        let mut index = InvertedIndex::new();
        index.add_doc("new home sales to forecasts".to_string());
        index.add_doc("home sales rise in july".to_string());
        index.add_doc("increase in home sales in july".to_string());
        index.add_doc("july new home sales rise".to_string());

        assert_eq!(index.words(), 9);
        assert_eq!(index.search_word("home"), Some(vec![0, 1, 2, 3]));
    }

    #[test]
    fn test_search() {
        let mut index = InvertedIndex::new();
        index.add_doc("new home sales to forecasts".to_string());
        index.add_doc("home sales rise in july".to_string());
        index.add_doc("increase in home sales in july".to_string());
        index.add_doc("july new home sales rise".to_string());

        // TODO query use .into()
        assert_eq!(index.search(Query::new("home AND rise").unwrap()), Ok(vec![1, 3]));
    }

    #[test]
    fn test_tokenize_query() {
        let tokens = tokenize("(home AND forecasts) OR increase").unwrap();
        let exp = vec![
            Token::OpenPar,
            Token::Word("home".to_string()),
            Token::And,
            Token::Word("forecasts".to_string()),
            Token::ClosePar,
            Token::Or,
            Token::Word("increase".to_string()),
        ];
        assert_eq!(tokens, exp);
    }

    #[test]
    fn test_parse_query() {
        let query = Query::new("(home AND forecasts) OR increase").unwrap();
        let exp_ast = AstNode::or(vec![
            AstNode::Word("increase".to_string()),
            AstNode::and(vec![AstNode::Word("forecasts".to_string()), AstNode::Word("home".to_string())])
        ]);
        assert_eq!(query.ast, exp_ast);
    }
}
