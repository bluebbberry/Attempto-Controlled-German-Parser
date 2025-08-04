package de.jbingemann.acg;

import java.util.*;
import java.util.regex.Pattern;
import java.util.regex.Matcher;
import java.util.stream.Collectors;

// AST Data Classes
abstract class Sentence {
    public abstract <T> T accept(SentenceVisitor<T> visitor);
}

class StatementSentence extends Sentence {
    public final Statement statement;
    public StatementSentence(Statement statement) { this.statement = statement; }
    public <T> T accept(SentenceVisitor<T> visitor) { return visitor.visitStatement(this); }
    @Override public String toString() { return "Statement(" + statement + ")"; }
    @Override public boolean equals(Object o) { return o instanceof StatementSentence && ((StatementSentence)o).statement.equals(statement); }
    @Override public int hashCode() { return statement.hashCode(); }
}

class QuestionSentence extends Sentence {
    public final Question question;
    public QuestionSentence(Question question) { this.question = question; }
    public <T> T accept(SentenceVisitor<T> visitor) { return visitor.visitQuestion(this); }
    @Override public String toString() { return "Question(" + question + ")"; }
    @Override public boolean equals(Object o) { return o instanceof QuestionSentence && ((QuestionSentence)o).question.equals(question); }
    @Override public int hashCode() { return question.hashCode(); }
}

class ConditionalSentence extends Sentence {
    public final Conditional conditional;
    public ConditionalSentence(Conditional conditional) { this.conditional = conditional; }
    public <T> T accept(SentenceVisitor<T> visitor) { return visitor.visitConditional(this); }
    @Override public String toString() { return "Conditional(" + conditional + ")"; }
    @Override public boolean equals(Object o) { return o instanceof ConditionalSentence && ((ConditionalSentence)o).conditional.equals(conditional); }
    @Override public int hashCode() { return conditional.hashCode(); }
}

class RuleSentence extends Sentence {
    public final Rule rule;
    public RuleSentence(Rule rule) { this.rule = rule; }
    public <T> T accept(SentenceVisitor<T> visitor) { return visitor.visitRule(this); }
    @Override public String toString() { return "Rule(" + rule + ")"; }
    @Override public boolean equals(Object o) { return o instanceof RuleSentence && ((RuleSentence)o).rule.equals(rule); }
    @Override public int hashCode() { return rule.hashCode(); }
}

interface SentenceVisitor<T> {
    T visitStatement(StatementSentence stmt);
    T visitQuestion(QuestionSentence quest);
    T visitConditional(ConditionalSentence cond);
    T visitRule(RuleSentence rule);
}

// Statement types
abstract class Statement {
    public abstract <T> T accept(StatementVisitor<T> visitor);
}

class SimpleStatement extends Statement {
    public final Subject subject;
    public final Predicate predicate;
    public final ACEObject object;

    public SimpleStatement(Subject subject, Predicate predicate, ACEObject object) {
        this.subject = subject;
        this.predicate = predicate;
        this.object = object;
    }

    public <T> T accept(StatementVisitor<T> visitor) { return visitor.visitSimple(this); }
    @Override public String toString() { return "SimpleStatement(" + subject + ", " + predicate + ", " + object + ")"; }
    @Override public boolean equals(Object o) {
        return o instanceof SimpleStatement &&
                ((SimpleStatement)o).subject.equals(subject) &&
                ((SimpleStatement)o).predicate.equals(predicate) &&
                ((SimpleStatement)o).object.equals(object);
    }
    @Override public int hashCode() { return Objects.hash(subject, predicate, object); }
}

class ExistentialStatement extends Statement {
    public final Subject subject;
    public final Predicate predicate;

    public ExistentialStatement(Subject subject, Predicate predicate) {
        this.subject = subject;
        this.predicate = predicate;
    }

    public <T> T accept(StatementVisitor<T> visitor) { return visitor.visitExistential(this); }
    @Override public String toString() { return "ExistentialStatement(" + subject + ", " + predicate + ")"; }
    @Override public boolean equals(Object o) {
        return o instanceof ExistentialStatement &&
                ((ExistentialStatement)o).subject.equals(subject) &&
                ((ExistentialStatement)o).predicate.equals(predicate);
    }
    @Override public int hashCode() { return Objects.hash(subject, predicate); }
}

class UniversalStatement extends Statement {
    public final Subject subject;
    public final Predicate predicate;
    public final ACEObject object;

    public UniversalStatement(Subject subject, Predicate predicate, ACEObject object) {
        this.subject = subject;
        this.predicate = predicate;
        this.object = object;
    }

    public <T> T accept(StatementVisitor<T> visitor) { return visitor.visitUniversal(this); }
    @Override public String toString() { return "UniversalStatement(" + subject + ", " + predicate + ", " + object + ")"; }
    @Override public boolean equals(Object o) {
        return o instanceof UniversalStatement &&
                ((UniversalStatement)o).subject.equals(subject) &&
                ((UniversalStatement)o).predicate.equals(predicate) &&
                ((UniversalStatement)o).object.equals(object);
    }
    @Override public int hashCode() { return Objects.hash(subject, predicate, object); }
}

class NegativeStatement extends Statement {
    public final Statement statement;

    public NegativeStatement(Statement statement) {
        this.statement = statement;
    }

    public <T> T accept(StatementVisitor<T> visitor) { return visitor.visitNegative(this); }
    @Override public String toString() { return "NegativeStatement(" + statement + ")"; }
    @Override public boolean equals(Object o) {
        return o instanceof NegativeStatement && ((NegativeStatement)o).statement.equals(statement);
    }
    @Override public int hashCode() { return statement.hashCode(); }
}

class PropertyStatement extends Statement {
    public final Subject subject;
    public final Property property;

    public PropertyStatement(Subject subject, Property property) {
        this.subject = subject;
        this.property = property;
    }

    public <T> T accept(StatementVisitor<T> visitor) { return visitor.visitProperty(this); }
    @Override public String toString() { return "PropertyStatement(" + subject + ", " + property + ")"; }
    @Override public boolean equals(Object o) {
        return o instanceof PropertyStatement &&
                ((PropertyStatement)o).subject.equals(subject) &&
                ((PropertyStatement)o).property.equals(property);
    }
    @Override public int hashCode() { return Objects.hash(subject, property); }
}

interface StatementVisitor<T> {
    T visitSimple(SimpleStatement stmt);
    T visitExistential(ExistentialStatement stmt);
    T visitUniversal(UniversalStatement stmt);
    T visitNegative(NegativeStatement stmt);
    T visitProperty(PropertyStatement stmt);
}

// Question types
abstract class Question {
    public abstract <T> T accept(QuestionVisitor<T> visitor);
}

class WhoQuestion extends Question {
    public final Predicate predicate;
    public final ACEObject object;

    public WhoQuestion(Predicate predicate, ACEObject object) {
        this.predicate = predicate;
        this.object = object;
    }

    public <T> T accept(QuestionVisitor<T> visitor) { return visitor.visitWho(this); }
    @Override public String toString() { return "WhoQuestion(" + predicate + ", " + object + ")"; }
    @Override public boolean equals(Object o) {
        return o instanceof WhoQuestion &&
                ((WhoQuestion)o).predicate.equals(predicate) &&
                ((WhoQuestion)o).object.equals(object);
    }
    @Override public int hashCode() { return Objects.hash(predicate, object); }
}

class YesNoQuestion extends Question {
    public final Statement statement;

    public YesNoQuestion(Statement statement) {
        this.statement = statement;
    }

    public <T> T accept(QuestionVisitor<T> visitor) { return visitor.visitYesNo(this); }
    @Override public String toString() { return "YesNoQuestion(" + statement + ")"; }
    @Override public boolean equals(Object o) {
        return o instanceof YesNoQuestion && ((YesNoQuestion)o).statement.equals(statement);
    }
    @Override public int hashCode() { return statement.hashCode(); }
}

class ListQuery extends Question {
    public final QueryType queryType;
    public final Constraint constraint;

    public ListQuery(QueryType queryType, Constraint constraint) {
        this.queryType = queryType;
        this.constraint = constraint;
    }

    public <T> T accept(QuestionVisitor<T> visitor) { return visitor.visitList(this); }
    @Override public String toString() { return "ListQuery(" + queryType + ", " + constraint + ")"; }
    @Override public boolean equals(Object o) {
        return o instanceof ListQuery &&
                ((ListQuery)o).queryType.equals(queryType) &&
                ((ListQuery)o).constraint.equals(constraint);
    }
    @Override public int hashCode() { return Objects.hash(queryType, constraint); }
}

class ExistenceQuery extends Question {
    public final Statement statement;

    public ExistenceQuery(Statement statement) {
        this.statement = statement;
    }

    public <T> T accept(QuestionVisitor<T> visitor) { return visitor.visitExistence(this); }
    @Override public String toString() { return "ExistenceQuery(" + statement + ")"; }
    @Override public boolean equals(Object o) {
        return o instanceof ExistenceQuery && ((ExistenceQuery)o).statement.equals(statement);
    }
    @Override public int hashCode() { return statement.hashCode(); }
}

interface QuestionVisitor<T> {
    T visitWho(WhoQuestion q);
    T visitYesNo(YesNoQuestion q);
    T visitList(ListQuery q);
    T visitExistence(ExistenceQuery q);
}

// Supporting types
enum QueryType { ALL_ENTITIES, ALL_PROPERTIES, ALL_RELATIONS }
enum Constraint { NO_CONSTRAINT, WITH_PROPERTY, WITH_RELATION }

class Conditional {
    public final Statement condition;
    public final Statement conclusion;

    public Conditional(Statement condition, Statement conclusion) {
        this.condition = condition;
        this.conclusion = conclusion;
    }

    @Override public String toString() { return "Conditional(" + condition + " -> " + conclusion + ")"; }
    @Override public boolean equals(Object o) {
        return o instanceof Conditional &&
                ((Conditional)o).condition.equals(condition) &&
                ((Conditional)o).conclusion.equals(conclusion);
    }
    @Override public int hashCode() { return Objects.hash(condition, conclusion); }
}

abstract class Rule {
    public abstract <T> T accept(RuleVisitor<T> visitor);
}

class ImplicationRule extends Rule {
    public final Statement premise;
    public final Statement conclusion;

    public ImplicationRule(Statement premise, Statement conclusion) {
        this.premise = premise;
        this.conclusion = conclusion;
    }

    public <T> T accept(RuleVisitor<T> visitor) { return visitor.visitImplication(this); }
    @Override public String toString() { return "ImplicationRule(" + premise + " -> " + conclusion + ")"; }
    @Override public boolean equals(Object o) {
        return o instanceof ImplicationRule &&
                ((ImplicationRule)o).premise.equals(premise) &&
                ((ImplicationRule)o).conclusion.equals(conclusion);
    }
    @Override public int hashCode() { return Objects.hash(premise, conclusion); }
}

interface RuleVisitor<T> {
    T visitImplication(ImplicationRule rule);
}

// Basic element types
abstract class Subject {
    public abstract String getName();
}

class ProperNoun extends Subject {
    public final String name;
    public ProperNoun(String name) { this.name = name; }
    public String getName() { return name; }
    @Override public String toString() { return "ProperNoun(" + name + ")"; }
    @Override public boolean equals(Object o) { return o instanceof ProperNoun && ((ProperNoun)o).name.equals(name); }
    @Override public int hashCode() { return name.hashCode(); }
}

class DefSubject extends Subject {
    public final Article article;
    public final Noun noun;

    public DefSubject(Article article, Noun noun) {
        this.article = article;
        this.noun = noun;
    }

    public String getName() { return noun.word; }
    @Override public String toString() { return "DefSubject(" + article + ", " + noun + ")"; }
    @Override public boolean equals(Object o) {
        return o instanceof DefSubject &&
                ((DefSubject)o).article.equals(article) &&
                ((DefSubject)o).noun.equals(noun);
    }
    @Override public int hashCode() { return Objects.hash(article, noun); }
}

class IndefSubject extends Subject {
    public final Article article;
    public final Noun noun;

    public IndefSubject(Article article, Noun noun) {
        this.article = article;
        this.noun = noun;
    }

    public String getName() { return noun.word; }
    @Override public String toString() { return "IndefSubject(" + article + ", " + noun + ")"; }
    @Override public boolean equals(Object o) {
        return o instanceof IndefSubject &&
                ((IndefSubject)o).article.equals(article) &&
                ((IndefSubject)o).noun.equals(noun);
    }
    @Override public int hashCode() { return Objects.hash(article, noun); }
}

class Variable extends Subject {
    public final String name;
    public Variable(String name) { this.name = name; }
    public String getName() { return "?" + name; }
    @Override public String toString() { return "Variable(" + name + ")"; }
    @Override public boolean equals(Object o) { return o instanceof Variable && ((Variable)o).name.equals(name); }
    @Override public int hashCode() { return name.hashCode(); }
}

abstract class ACEObject {
    public abstract String getName();
}

class ProperObject extends ACEObject {
    public final String name;
    public ProperObject(String name) { this.name = name; }
    public String getName() { return name; }
    @Override public String toString() { return "ProperObject(" + name + ")"; }
    @Override public boolean equals(Object o) { return o instanceof ProperObject && ((ProperObject)o).name.equals(name); }
    @Override public int hashCode() { return name.hashCode(); }
}

class DefObject extends ACEObject {
    public final Article article;
    public final Noun noun;

    public DefObject(Article article, Noun noun) {
        this.article = article;
        this.noun = noun;
    }

    public String getName() { return noun.word; }
    @Override public String toString() { return "DefObject(" + article + ", " + noun + ")"; }
    @Override public boolean equals(Object o) {
        return o instanceof DefObject &&
                ((DefObject)o).article.equals(article) &&
                ((DefObject)o).noun.equals(noun);
    }
    @Override public int hashCode() { return Objects.hash(article, noun); }
}

abstract class Predicate {
    public abstract String getName();
}

class VerbPredicate extends Predicate {
    public final Verb verb;
    public VerbPredicate(Verb verb) { this.verb = verb; }
    public String getName() { return verb.word; }
    @Override public String toString() { return "VerbPredicate(" + verb + ")"; }
    @Override public boolean equals(Object o) { return o instanceof VerbPredicate && ((VerbPredicate)o).verb.equals(verb); }
    @Override public int hashCode() { return verb.hashCode(); }
}

class AdjectivePredicate extends Predicate {
    public final Adjective adjective;
    public AdjectivePredicate(Adjective adjective) { this.adjective = adjective; }
    public String getName() { return adjective.word; }
    @Override public String toString() { return "AdjectivePredicate(" + adjective + ")"; }
    @Override public boolean equals(Object o) { return o instanceof AdjectivePredicate && ((AdjectivePredicate)o).adjective.equals(adjective); }
    @Override public int hashCode() { return adjective.hashCode(); }
}

// Basic word types
enum Article { DER, DIE, DAS, EIN, EINE, EINEN }

class Noun {
    public final String word;
    public Noun(String word) { this.word = word; }
    @Override public String toString() { return "Noun(" + word + ")"; }
    @Override public boolean equals(Object o) { return o instanceof Noun && ((Noun)o).word.equals(word); }
    @Override public int hashCode() { return word.hashCode(); }
}

class Verb {
    public final String word;
    public Verb(String word) { this.word = word; }
    @Override public String toString() { return "Verb(" + word + ")"; }
    @Override public boolean equals(Object o) { return o instanceof Verb && ((Verb)o).word.equals(word); }
    @Override public int hashCode() { return word.hashCode(); }
}

class Adjective {
    public final String word;
    public Adjective(String word) { this.word = word; }
    @Override public String toString() { return "Adjective(" + word + ")"; }
    @Override public boolean equals(Object o) { return o instanceof Adjective && ((Adjective)o).word.equals(word); }
    @Override public int hashCode() { return word.hashCode(); }
}

class Property {
    public final String name;
    public Property(String name) { this.name = name; }
    @Override public String toString() { return "Property(" + name + ")"; }
    @Override public boolean equals(Object o) { return o instanceof Property && ((Property)o).name.equals(name); }
    @Override public int hashCode() { return name.hashCode(); }
}

// Parser Exception
class ParseException extends Exception {
    public ParseException(String message) { super(message); }
}

// Knowledge Base
class KnowledgeBase {
    private final Set<Statement> facts;
    private final Set<Rule> rules;
    private final Set<String> entities;
    private final Set<Property> properties;

    public KnowledgeBase() {
        this.facts = new HashSet<>();
        this.rules = new HashSet<>();
        this.entities = new HashSet<>();
        this.properties = new HashSet<>();
    }

    public KnowledgeBase(Set<Statement> facts, Set<Rule> rules, Set<String> entities, Set<Property> properties) {
        this.facts = new HashSet<>(facts);
        this.rules = new HashSet<>(rules);
        this.entities = new HashSet<>(entities);
        this.properties = new HashSet<>(properties);
    }

    public void addFact(Statement fact) {
        facts.add(fact);
        extractEntities(fact);
    }

    public void addRule(Rule rule) {
        rules.add(rule);
    }

    private void extractEntities(Statement stmt) {
        stmt.accept(new StatementVisitor<Void>() {
            public Void visitSimple(SimpleStatement s) {
                entities.add(s.subject.getName());
                entities.add(s.object.getName());
                return null;
            }
            public Void visitExistential(ExistentialStatement s) {
                entities.add(s.subject.getName());
                return null;
            }
            public Void visitUniversal(UniversalStatement s) {
                entities.add(s.subject.getName());
                entities.add(s.object.getName());
                return null;
            }
            public Void visitNegative(NegativeStatement s) {
                s.statement.accept(this);
                return null;
            }
            public Void visitProperty(PropertyStatement s) {
                entities.add(s.subject.getName());
                properties.add(s.property);
                return null;
            }
        });
    }

    public Set<Statement> getFacts() { return new HashSet<>(facts); }
    public Set<Rule> getRules() { return new HashSet<>(rules); }
    public Set<String> getEntities() { return new HashSet<>(entities); }
    public Set<Property> getProperties() { return new HashSet<>(properties); }
}

// Deduction Engine
enum DeductionResult { PROVEN, DISPROVEN, UNKNOWN }

class DeductionEngine {
    private final KnowledgeBase kb;

    public DeductionEngine(KnowledgeBase kb) {
        this.kb = kb;
    }

    public DeductionResult deduce(Statement statement) {
        // Check if statement is directly in facts
        if (kb.getFacts().contains(statement)) {
            return DeductionResult.PROVEN;
        }

        // Check if negation exists
        for (Statement fact : kb.getFacts()) {
            if (contradicts(statement, fact)) {
                return DeductionResult.DISPROVEN;
            }
        }

        // Try to apply rules
        for (Rule rule : kb.getRules()) {
            if (rule instanceof ImplicationRule) {
                ImplicationRule implRule = (ImplicationRule) rule;
                if (implRule.conclusion.equals(statement) &&
                        deduce(implRule.premise) == DeductionResult.PROVEN) {
                    return DeductionResult.PROVEN;
                }
            }
        }

        return DeductionResult.UNKNOWN;
    }

    private boolean contradicts(Statement s1, Statement s2) {
        if (s1 instanceof NegativeStatement) {
            return ((NegativeStatement) s1).statement.equals(s2);
        }
        if (s2 instanceof NegativeStatement) {
            return ((NegativeStatement) s2).statement.equals(s1);
        }
        return false;
    }
}

// Query Processor
class QueryProcessor {
    private final DeductionEngine engine;
    private final KnowledgeBase kb;

    public QueryProcessor(KnowledgeBase kb) {
        this.kb = kb;
        this.engine = new DeductionEngine(kb);
    }

    public String processQuery(Question question) {
        return question.accept(new QuestionVisitor<String>() {
            public String visitWho(WhoQuestion q) {
                return "WHO query not fully implemented";
            }

            public String visitYesNo(YesNoQuestion q) {
                DeductionResult result = engine.deduce(q.statement);
                switch (result) {
                    case PROVEN: return "Ja";
                    case DISPROVEN: return "Nein";
                    case UNKNOWN: return "Unbekannt";
                    default: return "Fehler";
                }
            }

            public String visitList(ListQuery q) {
                switch (q.queryType) {
                    case ALL_ENTITIES:
                        return "Entitäten: " + kb.getEntities().stream()
                                .collect(Collectors.joining(", "));
                    case ALL_PROPERTIES:
                        return "Eigenschaften: " + kb.getProperties().stream()
                                .map(p -> p.name)
                                .collect(Collectors.joining(", "));
                    default:
                        return "Liste nicht implementiert";
                }
            }

            public String visitExistence(ExistenceQuery q) {
                DeductionResult result = engine.deduce(q.statement);
                return result == DeductionResult.PROVEN ? "Ja, existiert" : "Nein, existiert nicht";
            }
        });
    }
}

// German ACE Parser
public class GermanACEParser {
    private static final Map<String, Article> ARTICLES = Map.of(
            "der", Article.DER, "die", Article.DIE, "das", Article.DAS,
            "ein", Article.EIN, "eine", Article.EINE, "einen", Article.EINEN
    );

    private static final Set<String> VERBS = Set.of(
            "ist", "sind", "hat", "haben", "geht", "kommt", "lebt", "arbeitet"
    );

    private static final Set<String> ADJECTIVES = Set.of(
            "groß", "klein", "gut", "schlecht", "alt", "jung", "schnell", "langsam"
    );

    private static final Set<String> QUESTION_WORDS = Set.of("wer", "was", "wo");

    private final List<String> tokens;
    private int position;

    public GermanACEParser(String input) {
        this.tokens = tokenize(input);
        this.position = 0;
    }

    private List<String> tokenize(String input) {
        return Arrays.stream(input.toLowerCase()
                        .replaceAll("[.?!]", " $0 ")
                        .split("\\s+"))
                .filter(s -> !s.isEmpty())
                .collect(Collectors.toList());
    }

    public List<Sentence> parse() throws ParseException {
        List<Sentence> sentences = new ArrayList<>();

        while (!isAtEnd()) {
            sentences.add(parseSentence());
        }

        return sentences;
    }

    private Sentence parseSentence() throws ParseException {
        if (peek().equals("wenn")) {
            return new ConditionalSentence(parseConditional());
        } else if (QUESTION_WORDS.contains(peek()) || peek().equals("liste") || peek().equals("existiert")) {
            return new QuestionSentence(parseQuestion());
        } else if (peek().equals("nicht") && peekAhead(1).equals("alle")) {
            // This is a negative universal statement, treat as rule
            return new RuleSentence(parseRule());
        } else {
            return new StatementSentence(parseStatement());
        }
    }

    private Statement parseStatement() throws ParseException {
        if (peek().equals("nicht")) {
            advance(); // consume "nicht"
            return new NegativeStatement(parseStatement());
        }

        if (peek().equals("alle") || peek().equals("jeder") || peek().equals("jede")) {
            advance(); // consume quantifier
            Subject subject = parseSubject();
            advance(); // consume "die" or similar
            Predicate predicate = parsePredicate();
            ACEObject object = parseObject();
            return new UniversalStatement(subject, predicate, object);
        }

        Subject subject = parseSubject();
        Predicate predicate = parsePredicate();

        // Check if this is a property statement (subject ist adjective)
        if (predicate instanceof AdjectivePredicate) {
            Property property = new Property(((AdjectivePredicate) predicate).adjective.word);
            return new PropertyStatement(subject, property);
        }

        // Try to parse object
        if (!isAtEnd() && !peek().equals(".") && !peek().equals("?")) {
            ACEObject object = parseObject();
            return new SimpleStatement(subject, predicate, object);
        } else {
            return new ExistentialStatement(subject, predicate);
        }
    }

    private Question parseQuestion() throws ParseException {
        String questionWord = peek();

        if (questionWord.equals("wer")) {
            advance();
            Predicate predicate = parsePredicate();
            ACEObject object = parseObject();
            return new WhoQuestion(predicate, object);
        } else if (questionWord.equals("liste")) {
            advance();
            expect("alle");
            String queryTypeStr = advance();
            QueryType queryType;
            switch (queryTypeStr) {
                case "entitäten": queryType = QueryType.ALL_ENTITIES; break;
                case "eigenschaften": queryType = QueryType.ALL_PROPERTIES; break;
                default: throw new ParseException("Unknown query type: " + queryTypeStr);
            }
            return new ListQuery(queryType, Constraint.NO_CONSTRAINT);
        } else if (questionWord.equals("existiert")) {
            advance();
            Statement statement = parseStatement();
            return new ExistenceQuery(statement);
        } else {
            // Yes/no question - parse as statement
            Statement statement = parseStatement();
            return new YesNoQuestion(statement);
        }
    }

    private Conditional parseConditional() throws ParseException {
        expect("wenn");
        Statement condition = parseStatement();
        expect("dann");
        Statement conclusion = parseStatement();
        return new Conditional(condition, conclusion);
    }

    private Rule parseRule() throws ParseException {
        // For now, just parse implication rules
        Statement premise = parseStatement();
        if (peek().equals("->")) {
            advance();
            Statement conclusion = parseStatement();
            return new ImplicationRule(premise, conclusion);
        }
        throw new ParseException("Expected '->' in rule");
    }

    private Subject parseSubject() throws ParseException {
        String current = peek();

        if (ARTICLES.containsKey(current)) {
            Article article = ARTICLES.get(advance());
            Noun noun = new Noun(advance());
            return ARTICLES.get(article.toString().toLowerCase()).toString().startsWith("DER") ||
                    ARTICLES.get(article.toString().toLowerCase()).toString().startsWith("DIE") ||
                    ARTICLES.get(article.toString().toLowerCase()).toString().startsWith("DAS")
                    ? new DefSubject(article, noun) : new IndefSubject(article, noun);
        } else if (current.startsWith("?")) {
            return new Variable(advance().substring(1));
        } else if (Character.isUpperCase(current.charAt(0))) {
            return new ProperNoun(advance());
        } else {
            throw new ParseException("Expected subject, got: " + current);
        }
    }

    private ACEObject parseObject() throws ParseException {
        String current = peek();

        if (ARTICLES.containsKey(current)) {
            Article article = ARTICLES.get(advance());
            Noun noun = new Noun(advance());
            return new DefObject(article, noun);
        } else if (Character.isUpperCase(current.charAt(0))) {
            return new ProperObject(advance());
        } else {
            throw new ParseException("Expected object, got: " + current);
        }
    }

    private Predicate parsePredicate() throws ParseException {
        String current = peek();

        if (VERBS.contains(current)) {
            Verb verb = new Verb(advance());

            // Check if next word is adjective (for "ist groß" pattern)
            if (!isAtEnd() && ADJECTIVES.contains(peek())) {
                Adjective adjective = new Adjective(advance());
                return new AdjectivePredicate(adjective);
            }

            return new VerbPredicate(verb);
        } else {
            throw new ParseException("Expected predicate, got: " + current);
        }
    }

    // Helper methods
    private String peek() {
        if (isAtEnd()) return "";
        return tokens.get(position);
    }

    private String peekAhead(int offset) {
        if (position + offset >= tokens.size()) return "";
        return tokens.get(position + offset);
    }

    private String advance() {
        if (isAtEnd()) return "";
        return tokens.get(position++);
    }

    private boolean isAtEnd() {
        return position >= tokens.size();
    }

    private void expect(String expected) throws ParseException {
        if (!peek().equals(expected)) {
            throw new ParseException("Expected '" + expected + "', got '" + peek() + "'");
        }
        advance();
    }

    // Main parser entry point
    public static List<Sentence> parseGermanACE(String input) throws ParseException {
        GermanACEParser parser = new GermanACEParser(input);
        return parser.parse();
    }

    // Knowledge base builder
    public static KnowledgeBase buildKnowledgeBase(List<String> sentences) throws ParseException {
        KnowledgeBase kb = new KnowledgeBase();

        for (String sentence : sentences) {
            List<Sentence> parsed = parseGermanACE(sentence);
            for (Sentence s : parsed) {
                s.accept(new SentenceVisitor<Void>() {
                    public Void visitStatement(StatementSentence stmt) {
                        kb.addFact(stmt.statement);
                        return null;
                    }
                    public Void visitRule(RuleSentence rule) {
                        kb.addRule(rule.rule);
                        return null;
                    }
                    public Void visitQuestion(QuestionSentence q) { return null; }
                    public Void visitConditional(ConditionalSentence c) {
                        // Convert conditional to implication rule
                        kb.addRule(new ImplicationRule(c.conditional.condition, c.conditional.conclusion));
                        return null;
                    }
                });
            }
        }

        return kb;
    }

    // Query system entry point
    public static String processQuery(List<String> knowledgeBase, String queryString) {
        try {
            KnowledgeBase kb = buildKnowledgeBase(knowledgeBase);
            List<Sentence> queryParsed = parseGermanACE(queryString);

            if (queryParsed.size() == 1 && queryParsed.get(0) instanceof QuestionSentence) {
                QueryProcessor processor = new QueryProcessor(kb);
                Question question = ((QuestionSentence) queryParsed.get(0)).question;
                return processor.processQuery(question);
            } else {
                return "Invalid query format";
            }
        } catch (ParseException e) {
            return "Parse error: " + e.getMessage();
        }
    }

    // Example usage and test cases
    public static void main(String[] args) {
        System.out.println("German ACE Parser - Java Implementation");
        System.out.println("=====================================");

        // Test knowledge base
        List<String> exampleKB = Arrays.asList(
                "Der Mann ist groß.",
                "Peter ist ein Mann.",
                "Maria ist eine Frau.",
                "Alle Männer die groß sind sind stark.",
                "Wenn Peter groß ist dann ist Peter sichtbar."
        );

        // Test queries
        List<String> exampleQueries = Arrays.asList(
                "Ist Peter groß?",
                "Liste alle entitäten.",
                "Existiert ein großer Mann?",
                "Ist Maria stark?"
        );

        System.out.println("\nKnowledge Base:");
        for (String fact : exampleKB) {
            System.out.println("  " + fact);
        }

        System.out.println("\nQuery Processing Results:");
        for (String query : exampleQueries) {
            String result = processQuery(exampleKB, query);
            System.out.println("  Query: " + query);
            System.out.println("  Result: " + result);
            System.out.println();
        }

        // Test parsing individual sentences
        System.out.println("Individual Sentence Parsing Tests:");
        String[] testSentences = {
                "Der Mann ist groß.",
                "Peter arbeitet in München.",
                "Wer ist groß?",
                "Wenn der Mann alt ist dann ist er weise."
        };

        for (String sentence : testSentences) {
            try {
                List<Sentence> parsed = parseGermanACE(sentence);
                System.out.println("Input: " + sentence);
                System.out.println("Parsed: " + parsed);
                System.out.println();
            } catch (ParseException e) {
                System.out.println("Parse error for '" + sentence + "': " + e.getMessage());
            }
        }

        // Demonstration of deduction engine
        System.out.println("Deduction Engine Test:");
        try {
            KnowledgeBase kb = buildKnowledgeBase(exampleKB);
            DeductionEngine engine = new DeductionEngine(kb);

            // Test some deductions
            Statement testStatement = new PropertyStatement(
                    new ProperNoun("Peter"),
                    new Property("groß")
            );

            DeductionResult result = engine.deduce(testStatement);
            System.out.println("Can we deduce 'Peter ist groß'? " + result);

        } catch (ParseException e) {
            System.out.println("Error building knowledge base: " + e.getMessage());
        }
    }
}

// Utility class for pretty printing
class ACEPrinter {
    public static String printSentence(Sentence sentence) {
        return sentence.accept(new SentenceVisitor<String>() {
            public String visitStatement(StatementSentence stmt) {
                return printStatement(stmt.statement);
            }

            public String visitQuestion(QuestionSentence quest) {
                return printQuestion(quest.question);
            }

            public String visitConditional(ConditionalSentence cond) {
                return "Wenn " + printStatement(cond.conditional.condition) +
                        " dann " + printStatement(cond.conditional.conclusion);
            }

            public String visitRule(RuleSentence rule) {
                return printRule(rule.rule);
            }
        });
    }

    public static String printStatement(Statement statement) {
        return statement.accept(new StatementVisitor<String>() {
            public String visitSimple(SimpleStatement stmt) {
                return printSubject(stmt.subject) + " " +
                        printPredicate(stmt.predicate) + " " +
                        printObject(stmt.object);
            }

            public String visitExistential(ExistentialStatement stmt) {
                return printSubject(stmt.subject) + " " + printPredicate(stmt.predicate);
            }

            public String visitUniversal(UniversalStatement stmt) {
                return "Alle " + printSubject(stmt.subject) + " " +
                        printPredicate(stmt.predicate) + " " +
                        printObject(stmt.object);
            }

            public String visitNegative(NegativeStatement stmt) {
                return "Nicht " + printStatement(stmt.statement);
            }

            public String visitProperty(PropertyStatement stmt) {
                return printSubject(stmt.subject) + " ist " + stmt.property.name;
            }
        });
    }

    public static String printQuestion(Question question) {
        return question.accept(new QuestionVisitor<String>() {
            public String visitWho(WhoQuestion q) {
                return "Wer " + printPredicate(q.predicate) + " " + printObject(q.object) + "?";
            }

            public String visitYesNo(YesNoQuestion q) {
                return printStatement(q.statement) + "?";
            }

            public String visitList(ListQuery q) {
                return "Liste alle " + q.queryType.toString().toLowerCase();
            }

            public String visitExistence(ExistenceQuery q) {
                return "Existiert " + printStatement(q.statement) + "?";
            }
        });
    }

    public static String printRule(Rule rule) {
        return rule.accept(new RuleVisitor<String>() {
            public String visitImplication(ImplicationRule r) {
                return printStatement(r.premise) + " -> " + printStatement(r.conclusion);
            }
        });
    }

    private static String printSubject(Subject subject) {
        if (subject instanceof ProperNoun) {
            return ((ProperNoun) subject).name;
        } else if (subject instanceof DefSubject) {
            DefSubject ds = (DefSubject) subject;
            return ds.article.toString().toLowerCase() + " " + ds.noun.word;
        } else if (subject instanceof IndefSubject) {
            IndefSubject is = (IndefSubject) subject;
            return is.article.toString().toLowerCase() + " " + is.noun.word;
        } else if (subject instanceof Variable) {
            return "?" + ((Variable) subject).name;
        }
        return subject.toString();
    }

    private static String printObject(ACEObject object) {
        if (object instanceof ProperObject) {
            return ((ProperObject) object).name;
        } else if (object instanceof DefObject) {
            DefObject dobj = (DefObject) object;
            return dobj.article.toString().toLowerCase() + " " + dobj.noun.word;
        }
        return object.toString();
    }

    private static String printPredicate(Predicate predicate) {
        if (predicate instanceof VerbPredicate) {
            return ((VerbPredicate) predicate).verb.word;
        } else if (predicate instanceof AdjectivePredicate) {
            return "ist " + ((AdjectivePredicate) predicate).adjective.word;
        }
        return predicate.toString();
    }
}
