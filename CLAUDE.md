# CLAUDE.md - AI Assistant Guide

This document provides context and guidelines for AI assistants working on the **AI Impact on Workforce** project.

## Project Overview

**Repository:** `ai_impact_on_workforce`
**Purpose:** Research and documentation project analyzing the impact of artificial intelligence on the workforce, employment patterns, and labor markets.

**Status:** Early development phase - project structure and content are being established.

## Repository Structure

```
ai_impact_on_workforce/
├── CLAUDE.md          # AI assistant guidelines (this file)
├── readme.md          # Project README and documentation
└── .git/              # Git version control
```

### Planned Structure (as project grows)

```
ai_impact_on_workforce/
├── CLAUDE.md
├── readme.md
├── docs/              # Detailed documentation and reports
│   ├── research/      # Research papers and analysis
│   ├── data/          # Data documentation
│   └── methodology/   # Research methodology docs
├── src/               # Source code (if analysis tools are built)
│   ├── analysis/      # Data analysis scripts
│   ├── visualization/ # Charts and visualization code
│   └── utils/         # Utility functions
├── data/              # Data files (if applicable)
│   ├── raw/           # Raw data sources
│   └── processed/     # Cleaned/processed data
├── tests/             # Test files
└── outputs/           # Generated reports and visualizations
```

## Development Guidelines

### Git Workflow

1. **Branch Naming:** Use descriptive branch names
   - Feature branches: `feature/<description>`
   - Documentation: `docs/<description>`
   - Claude AI branches: `claude/<session-id>`

2. **Commit Messages:** Use clear, descriptive commit messages
   - Start with a verb (Add, Update, Fix, Remove, Refactor)
   - Keep the first line under 72 characters
   - Example: `Add analysis of AI adoption rates by industry sector`

3. **Push Commands:** Always use `git push -u origin <branch-name>`

### File Conventions

- **Markdown Files:** Use `.md` extension for documentation
- **Python Files:** Use `.py` extension, follow PEP 8 style guide
- **Data Files:** Prefer CSV for tabular data, JSON for structured data
- **Naming:** Use `snake_case` for files and directories

### Documentation Standards

- Keep documentation up-to-date with code changes
- Use clear headings and section organization
- Include sources and references for research claims
- Add timestamps or version notes to research documents

## Key Topics and Focus Areas

This project may cover:

1. **Employment Impact**
   - Job displacement and creation
   - Skill requirements evolution
   - Wage effects across sectors

2. **Industry Analysis**
   - Sector-specific AI adoption rates
   - Automation potential by occupation
   - Geographic distribution of impact

3. **Policy Considerations**
   - Workforce retraining programs
   - Social safety net adaptations
   - Regulatory frameworks

4. **Future Projections**
   - Short-term (1-5 years) forecasts
   - Long-term (10+ years) scenarios
   - Emerging AI capabilities and their implications

## Commands and Workflows

### Common Tasks

| Task | Command/Action |
|------|----------------|
| Check project status | `git status` |
| View recent changes | `git log --oneline -10` |
| Run Python analysis | `python src/analysis/<script>.py` (when applicable) |
| Generate documentation | Update relevant `.md` files |

### Research Workflow

1. Identify research question or topic
2. Gather data from reputable sources
3. Document methodology and sources
4. Perform analysis
5. Write findings in appropriate documentation
6. Commit and push changes

## Data Sources (Recommended)

When adding research content, prefer authoritative sources:

- Bureau of Labor Statistics (BLS)
- World Economic Forum reports
- OECD employment data
- Academic research papers (peer-reviewed)
- Industry reports from McKinsey, Deloitte, etc.

Always cite sources with:
- Author/Organization
- Publication date
- URL or DOI (when available)

## AI Assistant Instructions

### When Contributing to This Project

1. **Read First:** Review existing documentation before making changes
2. **Stay Focused:** Make targeted changes that address the specific task
3. **Document:** Update relevant documentation when adding new content
4. **Verify:** Double-check facts and statistics from reliable sources
5. **Organize:** Place files in appropriate directories per the structure above

### Best Practices

- Avoid speculation without data support
- Clearly distinguish between facts, analysis, and opinions
- Use neutral, objective language in research content
- Provide balanced perspectives on controversial topics
- Keep code (if any) simple and well-documented

### What to Avoid

- Making unsubstantiated claims about AI impact
- Ignoring data privacy considerations
- Adding dependencies without clear justification
- Creating overly complex file structures prematurely
- Pushing to branches other than the designated feature branch

## Current Development Priorities

As this is an early-stage project:

1. Establish clear project scope and objectives in readme.md
2. Create initial research documentation structure
3. Identify key data sources and research questions
4. Build foundational content before adding analysis tools

## Contact and Contribution

- **Repository Owner:** cyber-hbliu
- **Contributions:** Follow the git workflow above
- **Issues:** Report via GitHub issues

---

*Last updated: 2026-02-04*
