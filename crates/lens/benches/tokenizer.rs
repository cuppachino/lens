use criterion::{ criterion_group, criterion_main, Criterion };
use lens::tokens::{ prelude::*, tokenizer::Tokenizer };

/// recursively load all `.lens` files from a directory and its subdirectories
fn load_lens_files(dir: &str) -> Vec<(String, String)> {
    let mut files = Vec::new();
    for entry in std::fs::read_dir(dir).unwrap() {
        let entry = entry.unwrap();
        let path = entry.path();
        if path.is_file() && path.extension().map_or(false, |ext| ext == "lens") {
            let content = std::fs::read_to_string(&path).unwrap();
            let name = path.file_name().unwrap().to_string_lossy().to_string();
            files.push((name, content));
        } else if path.is_dir() {
            // Recursively load files from subdirectories
            let sub_files = load_lens_files(path.to_str().unwrap());
            files.extend(sub_files);
        }
    }
    files
}

fn bench_tokenizer(c: &mut Criterion) {
    let files = load_lens_files("bench_data/");

    for (name, content) in &files {
        c.bench_function(&format!("tokenize {}", name), |b| {
            b.iter(|| {
                Tokenizer::from_input(content).unwrap().collect::<Result<Vec<Token>, _>>().unwrap()
            });
        });
    }
}

criterion_group!(benches, bench_tokenizer);
criterion_main!(benches);

#[cfg(test)]
#[test]
fn test_load_lens_files() {
    let files = load_lens_files("bench_data/");
    assert!(!files.is_empty(), "No .lens files found");
    for (name, content) in &files {
        assert!(!name.is_empty(), "File name is empty");
        assert!(!content.is_empty(), "File content is empty");
    }
}
