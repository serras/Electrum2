package edu.mit.csail.sdg.alloy4whole;

import static edu.mit.csail.sdg.alloy4.A4Preferences.AutoVisualize;
import static edu.mit.csail.sdg.alloy4.A4Preferences.CoreGranularity;
import static edu.mit.csail.sdg.alloy4.A4Preferences.CoreMinimization;
import static edu.mit.csail.sdg.alloy4.A4Preferences.ImplicitThis;
import static edu.mit.csail.sdg.alloy4.A4Preferences.InferPartialInstance;
import static edu.mit.csail.sdg.alloy4.A4Preferences.NoOverflow;
import static edu.mit.csail.sdg.alloy4.A4Preferences.RecordKodkod;
import static edu.mit.csail.sdg.alloy4.A4Preferences.SkolemDepth;
import static edu.mit.csail.sdg.alloy4.A4Preferences.Solver;
import static edu.mit.csail.sdg.alloy4.A4Preferences.SubMemory;
import static edu.mit.csail.sdg.alloy4.A4Preferences.SubStack;
import static edu.mit.csail.sdg.alloy4.A4Preferences.Unrolls;
import static edu.mit.csail.sdg.alloy4.A4Preferences.VerbosityPref;
import static edu.mit.csail.sdg.alloy4.A4Preferences.WarningNonfatal;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectInputStream;
import java.io.UnsupportedEncodingException;
import java.net.URI;
import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Scanner;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import org.alloytools.alloy.core.AlloyCore;
import org.eclipse.core.runtime.URIUtil;
import org.eclipse.lsp4j.CodeActionParams;
import org.eclipse.lsp4j.CodeLens;
import org.eclipse.lsp4j.CodeLensOptions;
import org.eclipse.lsp4j.CodeLensParams;
import org.eclipse.lsp4j.Command;
import org.eclipse.lsp4j.CompletionItem;
import org.eclipse.lsp4j.CompletionList;
import org.eclipse.lsp4j.CompletionOptions;
import org.eclipse.lsp4j.CompletionParams;
import org.eclipse.lsp4j.Diagnostic;
import org.eclipse.lsp4j.DiagnosticSeverity;
import org.eclipse.lsp4j.DidChangeTextDocumentParams;
import org.eclipse.lsp4j.DidCloseTextDocumentParams;
import org.eclipse.lsp4j.DidOpenTextDocumentParams;
import org.eclipse.lsp4j.DidSaveTextDocumentParams;
import org.eclipse.lsp4j.DocumentFormattingParams;
import org.eclipse.lsp4j.DocumentHighlight;
import org.eclipse.lsp4j.DocumentOnTypeFormattingParams;
import org.eclipse.lsp4j.DocumentRangeFormattingParams;
import org.eclipse.lsp4j.DocumentSymbolParams;
import org.eclipse.lsp4j.Hover;
import org.eclipse.lsp4j.InitializeParams;
import org.eclipse.lsp4j.InitializeResult;
import org.eclipse.lsp4j.Location;
import org.eclipse.lsp4j.MessageParams;
import org.eclipse.lsp4j.MessageType;
import org.eclipse.lsp4j.Position;
import org.eclipse.lsp4j.PublishDiagnosticsParams;
import org.eclipse.lsp4j.Range;
import org.eclipse.lsp4j.ReferenceParams;
import org.eclipse.lsp4j.RenameParams;
import org.eclipse.lsp4j.ServerCapabilities;
import org.eclipse.lsp4j.SignatureHelp;
import org.eclipse.lsp4j.SymbolInformation;
import org.eclipse.lsp4j.TextDocumentIdentifier;
import org.eclipse.lsp4j.TextDocumentPositionParams;
import org.eclipse.lsp4j.TextDocumentSyncKind;
import org.eclipse.lsp4j.TextEdit;
import org.eclipse.lsp4j.WorkspaceEdit;
import org.eclipse.lsp4j.jsonrpc.messages.Either;
import org.eclipse.lsp4j.jsonrpc.services.JsonRequest;
import org.eclipse.lsp4j.services.LanguageClient;
import org.eclipse.lsp4j.services.LanguageClientAware;
import org.eclipse.lsp4j.services.LanguageServer;
import org.eclipse.lsp4j.services.TextDocumentService;
import org.eclipse.lsp4j.services.WorkspaceService;

import edu.mit.csail.sdg.alloy4.ConstList;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorSyntax;
import edu.mit.csail.sdg.alloy4.ErrorType;
import edu.mit.csail.sdg.alloy4.ErrorWarning;
import edu.mit.csail.sdg.alloy4.OurDialog;
import edu.mit.csail.sdg.alloy4.OurUtil;
import edu.mit.csail.sdg.alloy4.Pair;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4.Runner;
import edu.mit.csail.sdg.alloy4.Util;
import edu.mit.csail.sdg.alloy4.Version;
import edu.mit.csail.sdg.alloy4.WorkerEngine;
import edu.mit.csail.sdg.alloy4.WorkerEngine.WorkerCallback;
import edu.mit.csail.sdg.alloy4viz.VizGUI;
import edu.mit.csail.sdg.alloy4.A4Preferences.Verbosity;
import edu.mit.csail.sdg.alloy4whole.SimpleReporter.SimpleCallback1;
import edu.mit.csail.sdg.alloy4whole.SimpleReporter.SimpleTask1;
import edu.mit.csail.sdg.ast.Expr;
import edu.mit.csail.sdg.ast.ExprConstant;
import edu.mit.csail.sdg.ast.ExprVar;
import edu.mit.csail.sdg.ast.Sig;
import edu.mit.csail.sdg.ast.VisitQueryOnce;
import edu.mit.csail.sdg.parser.CompModule;
import edu.mit.csail.sdg.parser.CompUtil;
import edu.mit.csail.sdg.translator.A4Options;

import static edu.mit.csail.sdg.alloy4whole.Lsp4jUtil.*;

import static edu.mit.csail.sdg.alloy4whole.AlloyLSMessageType.*;
public class AlloyLanguageServer implements LanguageServer, LanguageClientAware {

	private AlloyLanguageClient client;
	private AlloyTextDocumentService alloyTextDocumentService;

	@Override
	public CompletableFuture<InitializeResult> initialize(InitializeParams params) {
		new Thread(() -> {
			while (true) {
				try {
					Thread.sleep(2000);
				} catch (InterruptedException e) {
				}

				// System.out.println("ping!");
			}
		}).start();
		InitializeResult res = new InitializeResult();

		ServerCapabilities caps = new ServerCapabilities();
		caps.setTextDocumentSync(Either.forLeft(TextDocumentSyncKind.Full));
		// caps.setCompletionProvider(new CompletionOptions(false, Arrays.asList(".", "
		// ", ",")));
		caps.setDefinitionProvider(true);
		CodeLensOptions codeLensProvider = new CodeLensOptions();
		// codeLensProvider.setResolveProvider(true);
		caps.setCodeLensProvider(codeLensProvider);

		res.setCapabilities(caps);
		return CompletableFuture.completedFuture(res);
	}

	@Override
	public CompletableFuture<Object> shutdown() {
		return CompletableFuture.completedFuture(null);
	}

	@Override
	public void exit() {
		System.exit(0);

	}

	@Override
	public TextDocumentService getTextDocumentService() {
		this.alloyTextDocumentService = new AlloyTextDocumentService(client);
		return alloyTextDocumentService;
	}

	@Override
	public WorkspaceService getWorkspaceService() {
		return null;
	}

	// LanguageClientAware
	@Override
	public void connect(LanguageClient client) {
		System.out.println("AlloyLanguageServer.connect() called");
		this.client = (AlloyLanguageClient) client;

		if (this.alloyTextDocumentService != null) {
			this.alloyTextDocumentService.client = this.client;
		}

	}

}

class AlloyTextDocumentService implements TextDocumentService, LanguageClientAware {

//    private String        text;
//    private String        uri;
	public AlloyLanguageClient client;
	private int subMemoryNow;
	private int subStackNow;

	public AlloyTextDocumentService(AlloyLanguageClient client) {
		this.client = client;
		System.out.println("AlloyTextDocumentService ctor, client: " + client);
	}

	// LanguageClientAware
	@Override
	public void connect(LanguageClient client) {
		System.out.println("AlloyTextDocumentService.connect() called");
		this.client = (AlloyLanguageClient) client;

	}

	@Override
	public CompletableFuture<Either<List<CompletionItem>, CompletionList>> completion(CompletionParams position) {
		List<CompletionItem> res = new ArrayList<>();
		try {
			CompModule module = CompUtil.parseOneModule(fileContents.get(position.getTextDocument().getUri()));
			module.getAllSigs().forEach(item -> res.add(new CompletionItem(item.label)));
		} catch (Exception ex) {
			System.out.println(ex.toString());
		}
		res.add(new CompletionItem("Hello!"));
		return CompletableFuture.completedFuture(Either.forLeft(res));
	}

	@Override
	public CompletableFuture<CompletionItem> resolveCompletionItem(CompletionItem unresolved) {
		return CompletableFuture.completedFuture(unresolved);
	}

	@Override
	public CompletableFuture<Hover> hover(TextDocumentPositionParams position) {
		Hover res = new Hover();

		return CompletableFuture.completedFuture(res);
	}

	@Override
	public CompletableFuture<SignatureHelp> signatureHelp(TextDocumentPositionParams position) {
		return CompletableFuture.completedFuture(new SignatureHelp());
	}


	@Override
	public CompletableFuture<List<? extends Location>> definition(TextDocumentPositionParams position) {
		try {
			CompModule module = CompUtil.parseOneModule(fileContents.get(position.getTextDocument().getUri()));

			
			Pos pos = positionToPos(position.getPosition(), null);
			System.out.println("definition request: module parsed! Request Position: " + pos.toString());

			Expr expr = module.find(pos);

			System.out.println("definition request for: " + expr);
			String exprName = getNameOf(expr);
			System.out.println("getNameOf(expr) == " + exprName);
			if (exprName != null) {
				for (Sig sig : module.getAllSigs()) {
					System.out.println("checking sig " + sig.label + "...");
					if (sigRemovePrefix(sig.label).equals(exprName)) {

						System.out.println("sig " + sig + " matched!");
						Location location = new Location();

						Range range = new Range();

						range.setStart(posToPosition(sig.pos));
						range.setEnd(range.getStart());
						location.setRange(range);
						// FIXME the implementation is not correct
						location.setUri(position.getTextDocument().getUri());
						return CompletableFuture.completedFuture(Arrays.asList(location));
					}
				}
			}
		}catch(Exception ex) {
			System.err.println("Error in providing definition: " + ex.toString());
		}
		return CompletableFuture.completedFuture(Arrays.asList());
		// edu.mit.csail.sdg.alloy4.OurSyntaxWidget.doNav()
	}

	@Override
	public CompletableFuture<List<? extends Location>> references(ReferenceParams params) {
		return CompletableFuture.completedFuture(Arrays.asList());
	}

	@Override
	public CompletableFuture<List<? extends DocumentHighlight>> documentHighlight(TextDocumentPositionParams position) {
		return CompletableFuture.completedFuture(Arrays.asList());
	}

	@Override
	public CompletableFuture<List<? extends SymbolInformation>> documentSymbol(DocumentSymbolParams params) {
		return CompletableFuture.completedFuture(Arrays.asList());
	}

	@Override
	public CompletableFuture<List<? extends Command>> codeAction(CodeActionParams params) {
		// params.getContext().getDiagnostics().get(0).
		return CompletableFuture.completedFuture(null);
	}

	@Override
	public CompletableFuture<List<? extends CodeLens>> codeLens(CodeLensParams params) {

		String uri = params.getTextDocument().getUri();
		String text = fileContents.get(uri);
		CompModule module;
		try {
			module = CompUtil.parseOneModule(text);	
			
		} catch (ErrorSyntax err) {
			org.eclipse.lsp4j.Diagnostic diag = newDiagnostic(err.msg, createRangeFromPos(err.pos));
			diag.setSeverity(DiagnosticSeverity.Error);
			
			PublishDiagnosticsParams diagnosticsParams = 
					newPublishDiagnosticsParams(uri, Arrays.asList(diag));
			
			
			fileUrisOfLastPublishedDiagnostics = new ArrayList<>(fileUrisOfLastPublishedDiagnostics);
			fileUrisOfLastPublishedDiagnostics.add(uri);
			
			latestFileUrisWithSyntaxError.add(uri);
			client.publishDiagnostics(diagnosticsParams );
			
			return CompletableFuture.completedFuture(Arrays.asList());
		}
		ConstList<edu.mit.csail.sdg.ast.Command> commands = module.getAllCommands();
		
		ArrayList<CodeLens> res = new ArrayList<>();

		for (int i = 0; i < commands.size(); i++) {
			edu.mit.csail.sdg.ast.Command command = commands.get(i);
			Position position = posToPosition(command.pos);
			CodeLens codeLens = new CodeLens();
			codeLens.setRange(createRange(position, position));
			Command vsCommand = new Command("Execute", "ExecuteAlloyCommand"); // ExecuteAlloyCommand
			vsCommand.setArguments(
					Arrays.asList(uri, i, position.getLine(), position.getCharacter()));
			codeLens.setCommand(vsCommand);

			res.add(codeLens);
		}
		if (latestFileUrisWithSyntaxError.contains(uri)) {
			client.publishDiagnostics(newPublishDiagnosticsParams(uri, Arrays.asList()));
			latestFileUrisWithSyntaxError.remove(uri);
		}
		return CompletableFuture.completedFuture(res);
	}
	
	final Set<String> latestFileUrisWithSyntaxError = new HashSet<>();

	@JsonRequest
	public CompletableFuture<Void> ExecuteAlloyCommand(com.google.gson.JsonArray params) {
		CompletableFuture<Void> res = CompletableFuture.completedFuture(null);
		
		System.out.println("ExecuteAlloyCommand() called with " + params + ", " + params.getClass());
		String uri = params.get(0).getAsString();
		int ind = params.get(1).getAsInt();
		int line = params.get(2).getAsInt(), character = params.get(3).getAsInt();

		Position position = new Position(line, character);
		Pos pos = positionToPos(position);

		CompModule module = CompUtil.parseOneModule(fileContents.get(uri));
		ConstList<edu.mit.csail.sdg.ast.Command> commands = module.getAllCommands();

		edu.mit.csail.sdg.ast.Command command = commands.stream()
				.filter(comm -> comm.pos().y == pos.y && comm.pos.x == pos.x).findFirst().orElse(null);
		if (command != null) {
			// System.out.println("ExecuteAlloyCommand() called with " + position);
			MessageParams messageParams = new MessageParams();
			messageParams.setMessage("executing " + command.label);
			//client.showMessage(messageParams);

			res.thenRunAsync(() -> doRun(uri, ind));
		} else {
			System.err.println("no matching command found");
		}

		return res;
	}
	
	@JsonRequest
	public CompletableFuture<Void> StopExecution(Object o) {
		System.out.println("Stop req received");
		doStop(2);
		
		AlloyLSMessage alloyMsg = new AlloyLSMessage(RunCompleted, "Stopped");
		alloyMsg.bold = true;
		client.showExecutionOutput(alloyMsg);

		return CompletableFuture.completedFuture(null);
	}
	
	@JsonRequest
	public CompletableFuture<Void> OpenModel(com.google.gson.JsonPrimitive link) {
		System.out.println("OpenModel() called with " + link.getAsString());
		System.out.println(" , of type" + link.getClass().getName());
		doVisualize(link.getAsString());
		return CompletableFuture.completedFuture(null);
	}
	
	@JsonRequest
	public CompletableFuture<Void> ListAlloyCommands(com.google.gson.JsonPrimitive documentUri){
		CodeLensParams params = new CodeLensParams();
		TextDocumentIdentifier textDocument = new TextDocumentIdentifier();
		textDocument.setUri(documentUri.getAsString());
		params.setTextDocument(textDocument );
		List<? extends CodeLens> codeLensRes = uncheckedRun( () -> codeLens(params).get());
		codeLensRes.stream().map( codeLens -> {
			return codeLens.getCommand().getArguments();
		});
		
		return null;
	}


	static Pos positionToPos(Position position) {
		return positionToPos(position, null);
	}

	static Pos positionToPos(Position position, String fileName) {
		return new Pos(fileName, position.getCharacter() + 1, position.getLine() + 1);
	}

	static org.eclipse.lsp4j.Position posToPosition(edu.mit.csail.sdg.alloy4.Pos pos) {
		Position position = new Position();
		position.setLine(pos.y - 1);
		position.setCharacter(pos.x - 1);
		return position;
	}
	static Range createRange(Position start, Position end) {
		Range res = new Range();
		res.setStart(start);
		res.setEnd(end);
		return res;
	}
	static Range createRangeFromPos(Pos pos) {
		Range res = new Range();
		res.setStart(posToPosition(pos));
		
		Position endPosition = new Position();
		endPosition.setLine(pos.y2 - 1);
		endPosition.setCharacter(pos.x2 - 1);
		
		res.setEnd(endPosition);
		return res;
	}

	@Override
	public CompletableFuture<CodeLens> resolveCodeLens(CodeLens unresolved) {
		return CompletableFuture.completedFuture(unresolved);
	}

	@Override
	public CompletableFuture<List<? extends TextEdit>> formatting(DocumentFormattingParams params) {
		return CompletableFuture.completedFuture(null);
	}

	@Override
	public CompletableFuture<List<? extends TextEdit>> rangeFormatting(DocumentRangeFormattingParams params) {
		return CompletableFuture.completedFuture(null);
	}

	@Override
	public CompletableFuture<List<? extends TextEdit>> onTypeFormatting(DocumentOnTypeFormattingParams params) {
		return CompletableFuture.completedFuture(null);
	}

	@Override
	public CompletableFuture<WorkspaceEdit> rename(RenameParams params) {
		return CompletableFuture.completedFuture(null);
	}

	private HashMap<String, String> fileContents = new HashMap<>();

	@Override
	public void didOpen(DidOpenTextDocumentParams params) {
		String text = params.getTextDocument().getText();
		String uri = params.getTextDocument().getUri();
		fileContents.put(uri, text);
		// System.out.println("doc: \n" + this.text);
		System.out.println(params.getTextDocument().getUri());
	}

	@Override
	public void didChange(DidChangeTextDocumentParams params) {
		// https://raw.githubusercontent.com/eclipse/eclipse.jdt.ls/0ed1bf259b3edb4f184804a9df14c95ef468d4e9/org.eclipse.jdt.ls.core/src/org/eclipse/jdt/ls/core/internal/handlers/DocumentLifeCycleHandler.java
		System.out.println("didChange: \n" + params.getTextDocument().getUri());
		String text = params.getContentChanges().get(0).getText();
		String uri = params.getTextDocument().getUri();
		fileContents.put(uri, text);
		// System.out.println("doc: \n" + this.text);
	}

	@Override
	public void didClose(DidCloseTextDocumentParams params) {

	}

	@Override
	public void didSave(DidSaveTextDocumentParams params) {

	}

	static String sigRemovePrefix(String sig) {
		if (sig.startsWith("this/")) {
			return sig.substring("this/".length());
		}
		return sig;
	}

	/**
	 * returns the referenced variable name if the expression references a variable
	 *
	 * @param expr
	 */
	public static String getNameOf(Expr expr) {
		VisitQueryOnce<String> visitor = new VisitQueryOnce<String>() {

			@Override
			public String visit(ExprVar x) throws Err {
				return x.label;
			}

			@Override
			public String visit(ExprConstant x) throws Err {
				return x.string;
			}
		};
		return visitor.visitThis(expr);
	}

	private static String filePathToUri(String absolutePath) {
		return Paths.get(absolutePath).toUri().toString();
	}
	private List<PublishDiagnosticsParams> toPublishDiagnosticsParamsList(List<Err> warnings){
		
		Map<String, List<Err>> map =
				warnings.stream()
				.collect(Collectors.groupingBy( warning -> warning.pos.filename));
		return map.entrySet().stream().map(entry -> {

			List<Diagnostic> diags= entry.getValue().stream().map(err -> {
				Diagnostic diag = newDiagnostic(err.msg, createRangeFromPos(err.pos));
				diag.setSeverity(err instanceof ErrorWarning ? 
						DiagnosticSeverity.Warning : DiagnosticSeverity.Error);
				
				return diag;
			}).collect(Collectors.toList());

			return newPublishDiagnosticsParams(filePathToUri(entry.getKey()), diags);
		}).collect(Collectors.toList());		
	}
	
	private Collection<String> fileUrisOfLastPublishedDiagnostics = null;
	// edu.mit.csail.sdg.alloy4whole.SimpleGUI.doRun(Integer)
	private void doRun(String fileURI, Integer commandIndex) {
		CompModule module = CompUtil.parseOneModule(fileContents.get(fileURI));
		ConstList<edu.mit.csail.sdg.ast.Command> commands = module.getAllCommands();

		final int index = commandIndex;
		if (WorkerEngine.isBusy())
			return;

		if (commands == null)
			return;
		if (commands.size() == 0 && index != -2 && index != -3) {
			// log.logRed("There are no commands to execute.\n\n");
			return;
		}
		int i = index;
		if (i >= commands.size())
			i = commands.size() - 1;
		// SimpleCallback1 cb = new SimpleCallback1(this, null, log,
		// VerbosityPref.get().ordinal(), latestAlloyVersionName, latestAlloyVersion);
		SimpleTask1 task = new SimpleTask1();
		A4Options opt = new A4Options();
		opt.tempDirectory = alloyHome() + fs + "tmp";
		opt.solverDirectory = alloyHome() + fs + "binary";
		opt.recordKodkod = RecordKodkod.get();
		opt.noOverflow = NoOverflow.get();
		opt.unrolls = Version.experimental ? Unrolls.get() : (-1);
		opt.skolemDepth = SkolemDepth.get();
		opt.coreMinimization = CoreMinimization.get();
		opt.inferPartialInstance = InferPartialInstance.get();
		opt.coreGranularity = CoreGranularity.get();

		System.out.println("cwd :" + Paths.get("").toAbsolutePath());
		System.out.println("uri :" + decodeUrl(fileURI));

		String fileNameDecoded;
		try {
			fileNameDecoded = URIUtil.toFile(new URI(fileURI)).getPath();
			
		} catch (Exception ex) {
			System.err.println("failed to parse uri");
			return;
		}
		System.out.println("fileNameDecoded:" + fileNameDecoded);

		opt.originalFilename = fileNameDecoded;// new File(decodeUrl(uri)).getPath();// Util.canon(decodeUrl(uri));
		opt.solver = Solver.get();
		task.bundleIndex = i;
		task.bundleWarningNonFatal = WarningNonfatal.get();
		// task.map = text.takeSnapshot();
		task.options = opt.dup();
		task.resolutionMode = (Version.experimental && ImplicitThis.get()) ? 2 : 1;
		task.tempdir = maketemp();

		try {
			int newmem = SubMemory.get(), newstack = SubStack.get();
			if (newmem != subMemoryNow || newstack != subStackNow)
				WorkerEngine.stop();

			List<Err> warnings = new ArrayList<>();
			WorkerCallback cb = new WorkerCallback() {

				@Override
				public void callback(Object msg) {

					// MessageParams messageParams= new MessageParams();
					// messageParams.setMessage(solverCallbackMsgToString(msg));
					// client.showMessage(messageParams);
//					if(messageParams.getMessage() != null && !messageParams.getMessage().isEmpty())
//						client.showAlloyOutput(messageParams);

					Either<List<AlloyLSMessage>, Err> alloyMsgOrWarning =
							solverCallbackMsgToAlloyMsg(msg);

					if (alloyMsgOrWarning.isLeft()) {
						for(AlloyLSMessage alloyMsg :alloyMsgOrWarning.getLeft()) {
							if (alloyMsg.message != null && !alloyMsg.message.isEmpty())
								client.showExecutionOutput(alloyMsg);
						}

					} else {
						warnings.add(alloyMsgOrWarning.getRight());
					}
				}

				@Override
				public void done() {
					// MessageParams messageParams= new MessageParams();
					// messageParams.setMessage("done");
					// client.showMessage(messageParams);
					// client.showAlloyOutput(messageParams);
					if(warnings.size() > 0)
						client.showExecutionOutput(
							new AlloyLSMessage(AlloyLSMessageType.RunResult, "There were errors/warnings!"));
					
					client.showExecutionOutput(new AlloyLSMessage(AlloyLSMessageType.RunCompleted, ""));
					
					publishDiagnostics();
				}

				@Override
				public void fail() {
					AlloyLSMessage alloyMsg = new AlloyLSMessage(
							AlloyLSMessageType.RunCompleted, "failure");
					client.showExecutionOutput(alloyMsg);
					publishDiagnostics();
				}
				
				void publishDiagnostics() {
					
					//cleaning previously reported diagnostics
					if(fileUrisOfLastPublishedDiagnostics != null)
						fileUrisOfLastPublishedDiagnostics.stream().forEach(item -> {
							PublishDiagnosticsParams diagnostics = new PublishDiagnosticsParams();
							diagnostics.setUri(item);
							client.publishDiagnostics(diagnostics );
					});

					List<PublishDiagnosticsParams> diagnosticsParamsList = 
							toPublishDiagnosticsParamsList(warnings);
					
					diagnosticsParamsList
						.forEach(diagsParams -> client.publishDiagnostics(diagsParams));

					fileUrisOfLastPublishedDiagnostics = 
							diagnosticsParamsList.stream()
							.map( item -> item.getUri())
							.collect(Collectors.toList());

				}

			};

			System.out.println("actually running the task");
			// if (AlloyCore.isDebug() && VerbosityPref.get() == Verbosity.FULLDEBUG)
			//WorkerEngine.runLocally(task, cb);
			// else
			WorkerEngine.run(task, newmem, newstack, alloyHome() + fs + "binary", "", cb);
			subMemoryNow = newmem;
			subStackNow = newstack;
		} catch (Throwable ex) {
			WorkerEngine.stop();
			System.err.println("Fatal Error: Solver failed due to unknown reason. exception: \n" + ex.toString());
			// log.logBold("Fatal Error: Solver failed due to unknown reason.\n" + "One
			// possible cause is that, in the Options menu, your specified\n" + "memory size
			// is larger than the amount allowed by your OS.\n" + "Also, please make sure
			// \"java\" is in your program path.\n");
			// log.logDivider();
			// log.flush();
			doStop(2);
		}
		return;
	}

	private static String decodeUrl(String value) {
		try {
			return URLDecoder.decode(value, StandardCharsets.UTF_8.toString());
		} catch (UnsupportedEncodingException e) {
			return value;
		}
	}

	/**
	 * The system-specific file separator (forward-slash on UNIX, back-slash on
	 * Windows, etc.)
	 */
	private static final String fs = System.getProperty("file.separator");

	/**
	 * This variable caches the result of alloyHome() function call.
	 */
	private static String alloyHome = null;

	/**
	 * Find a temporary directory to store Alloy files; it's guaranteed to be a
	 * canonical absolute path.
	 */
	private static synchronized String alloyHome() {
		if (alloyHome != null)
			return alloyHome;
		String temp = System.getProperty("java.io.tmpdir");
		if (temp == null || temp.length() == 0)
			OurDialog.fatal("Error. JVM need to specify a temporary directory using java.io.tmpdir property.");
		String username = System.getProperty("user.name");
		File tempfile = new File(temp + File.separatorChar + "alloy4tmp40-" + (username == null ? "" : username));
		tempfile.mkdirs();
		String ans = Util.canon(tempfile.getPath());
		if (!tempfile.isDirectory()) {
			OurDialog.fatal("Error. Cannot create the temporary directory " + ans);
		}
		if (!Util.onWindows()) {
			String[] args = { "chmod", "700", ans };
			try {
				Runtime.getRuntime().exec(args).waitFor();
			} catch (Throwable ex) {
			} // We only intend to make a best effort.
		}
		return alloyHome = ans;
	}

	/**
	 * Create an empty temporary directory for use, designate it "deleteOnExit",
	 * then return it. It is guaranteed to be a canonical absolute path.
	 */
	private static String maketemp() {
		Random r = new Random(new Date().getTime());
		while (true) {
			int i = r.nextInt(1000000);
			String dest = alloyHome() + File.separatorChar + "tmp" + File.separatorChar + i;
			File f = new File(dest);
			if (f.mkdirs()) {
				f.deleteOnExit();
				return Util.canon(dest);
			}
		}
	}

	// edu.mit.csail.sdg.alloy4whole.SimpleReporter.SimpleCallback1.callback(Object)
	public Either<List<AlloyLSMessage>, Err> solverCallbackMsgToAlloyMsg(Object msg) {
		StringBuilder span = new StringBuilder();
		AlloyLSMessage alloyMsg = new AlloyLSMessage(
				AlloyLSMessageType.RunInProgress, null);
		
		List<AlloyLSMessage> resMsgs = new ArrayList<>();
		resMsgs.add(alloyMsg);
		
		final int verbosity = 0;
		if (msg == null) {
			span.append("Done\n");
		} else if (msg instanceof String) {
			span.append(((String) msg).trim() + "\n");
		} else if (msg instanceof Throwable) {
			for (Throwable ex = (Throwable) msg; ex != null; ex = ex.getCause()) {
				if (ex instanceof OutOfMemoryError) {
					span.append("\nFatal Error: the solver ran out of memory!\n"
							+ "Try simplifying your model or reducing the scope,\n"
							+ "or increase memory under the Options menu.\n");
				}
				if (ex instanceof StackOverflowError) {
					span.append("\nFatal Error: the solver ran out of stack space!\n"
							+ "Try simplifying your model or reducing the scope,\n"
							+ "or increase stack under the Options menu.\n");
				}
			}
			if (msg instanceof Err) {
				System.out.println("Err msg from solver: " + msg.toString());
				Err ex = (Err) msg;
				String text = "fatal";
				boolean fatal = false;
				if (ex instanceof ErrorSyntax)
					text = "syntax";
				else if (ex instanceof ErrorType)
					text = "type";
				else
					fatal = true;
				if (ex.pos == Pos.UNKNOWN)
					span.append("A " + text + " error has occurred:  ");
				else
					span.append("A " + text + " error has occurred:  " + "POS: " + ex.pos.x + " " + ex.pos.y + " "
							+ ex.pos.x2 + " " + ex.pos.y2 + " " + ex.pos.filename);
//            if (verbosity > 2) {
//                span.log("(see the ");
//                span.logLink("stacktrace", "MSG: " + ex.dump());
//                span.log(")\n");
//            } else {
//                span.log("\n");
//            }
				span.append(ex.msg.trim()); // logIndented
				return Either.forRight(ex);
				//span.append("\n");
//            if (fatal && latestVersion > Version.buildNumber())
//                span.logBold("\nNote: You are running Alloy build#" + Version.buildNumber() + ",\nbut the most recent is Alloy build#" + latestVersion + ":\n( version " + latestName + " )\nPlease try to upgrade to the newest version," + "\nas the problem may have been fixed already.\n");

//            if (!fatal)
//                gui.doVisualize("POS: " + ex.pos.x + " " + ex.pos.y + " " + ex.pos.x2 + " " + ex.pos.y2 + " " + ex.pos.filename);
			}
			else /*if (msg instanceof Throwable)*/ {
				System.out.println("exception msg from solver: " + msg.toString());
				Throwable ex = (Throwable) msg;
				span.append(ex.toString().trim() + "\n");
				// span.flush();
			}
		 } else if ((msg instanceof Object[])) {
			Object[] array = (Object[]) msg;
			if (array[0].equals("pop")) {
				// span.setLength(len2);
				String x = (String) (array[1]);
				span.append(x);
				alloyMsg.replaceLast = true;
//            if (viz != null && x.length() > 0)
//                OurDialog.alert(x);
			}
			if (array[0].equals("declare")) {
//            gui.doSetLatest((String) (array[1]));
			}
			if (array[0].equals("S2")) {
//            len3 = len2 = span.getLength();
				span.append("" + array[1]);
			}
			if (array[0].equals("R3")) {
//            span.setLength(len3);
				span.append("" + array[1]);
			}
			if (array[0].equals("link")) {
				System.out.println("link message!!!");
//            span.logLink((String) (array[1]), (String) (array[2]));
				span.append(array[1].toString());
				alloyMsg.link = (String) array[2];
				alloyMsg.messageType = AlloyLSMessageType.RunResult;

			}
			if (array[0].equals("bold")) {
				span.append("" + array[1]);
				alloyMsg.bold = true;
				alloyMsg.replaceLast = true;
			}
			if (array[0].equals("")) {
				span.append("" + array[1]);
			}
			if (array[0].equals("scope") && verbosity > 0) {
				span.append("   " + array[1]);
			}
			if (array[0].equals("bound") && verbosity > 1) {
				span.append("   " + array[1]);
			}
			if (array[0].equals("resultCNF")) {
				// results.add(null);
				// span.setLength(len3);
				// span.log(" File written to " + array[1] + "\n\n");
			}
			if (array[0].equals("debug") && verbosity > 2) {
				span.append("   " + array[1] + "\n");
				// len2 = len3 = span.getLength();
			}
			if (array[0].equals("translate")) {
				span.append("   " + array[1]);
				// len3 = span.getLength();
				span.append("   Generating CNF...\n");
			}
			if (array[0].equals("solve")) {
				// span.setLength(len3);
				span.append("   " + array[1]);
				// len3 = span.getLength();
				span.append("   Solving...\n");
			}
			if (array[0].equals("warnings")) {
//            if (warnings.size() == 0)
//                span.setLength(len2);
//            else if (warnings.size() > 1)
//                span.logBold("Note: There were " + warnings.size() + " compilation warnings. Please scroll up to see them.\n\n");
//            else
				// span.append("Note: There was 1 compilation warning. Please scroll up to see
				// them.\n\n");
				 //span.append("There were warnings!");
//            if (warnings.size() > 0 && Boolean.FALSE.equals(array[1])) {
//                Pos e = warnings.iterator().next().pos;
//                gui.doVisualize("POS: " + e.x + " " + e.y + " " + e.x2 + " " + e.y2 + " " + e.filename);
//                span.logBold("Warnings often indicate errors in the model.\n" + "Some warnings can affect the soundness of the analysis.\n" + "To proceed despite the warnings, go to the Options menu.\n");
//            }
			}
			if (array[0].equals("warning")) {
				ErrorWarning e = (ErrorWarning) (array[1]);
//            if (!warnings.add(e))
//                return;
				Pos p = e.pos;
//            span.logLink("Warning #" + warnings.size(), "POS: " + p.x + " " + p.y + " " + p.x2 + " " + p.y2 + " " + p.filename);
				span.append("Warning " + e.msg.trim());// #" + warnings.size(), "POS: " + p.x + " " + p.y + " " + p.x2 +
														// " " + p.y2 + " " + p.filename);
				return Either.forRight(e);
//            span.log("\n");
//            span.logIndented(e.msg.trim());
//            span.log("\n\n");
			}
			if (array[0].equals("sat")) {
				boolean chk = Boolean.TRUE.equals(array[1]);
				int expects = (Integer) (array[2]);
				String filename = (String) (array[3]), formula = (String) (array[4]);
				// results.add(filename);
				(new File(filename)).deleteOnExit();
				// gui.doSetLatest(filename);
				// span.setLength(len3);
				span.append("   ");
				// span.logLink(chk ? "Counterexample" : "Instance", "XML: " + filename);
				span.append(chk ? "Counterexample" : "Instance");// , "XML: " + filename);
				alloyMsg.link = "XML: " + filename;
				alloyMsg.messageType = AlloyLSMessageType.RunResult;
				alloyMsg.replaceLast = true;
				alloyMsg.bold = true;
				span.append(" found. ");
				// span.logLink(chk ? "Assertion" : "Predicate", formula);
				span.append(chk ? "Assertion" : "Predicate");// , formula);
				span.append(chk ? " is invalid" : " is consistent");
				if (expects == 0)
					span.append(", contrary to expectation");
				else if (expects == 1)
					span.append(", as expected");
				span.append(".");
				//span.append(". " + array[5] + "ms.\n\n");
				alloyMsg.lineBreak = false;
				resMsgs.add(new AlloyLSMessage(AlloyLSMessageType.RunResult, array[5] + "ms.\n\n"));
			}
			if (array[0].equals("metamodel")) {
//            String outf = (String) (array[1]);
//            span.setLength(len2);
//            (new File(outf)).deleteOnExit();
//            gui.doSetLatest(outf);
//            span.logLink("Metamodel", "XML: " + outf);
//            span.log(" successfully generated.\n\n");
			}
			if (array[0].equals("minimizing")) {
              boolean chk = Boolean.TRUE.equals(array[1]);
//            int expects = (Integer) (array[2]);
//            span.setLength(len3);
//            span.log(chk ? "   No counterexample found." : "   No instance found.");
              span.append(chk ? "   No counterexample found." : "   No instance found.");
            if (chk)
                span.append(" Assertion may be valid");
            else
                span.append(" Predicate may be inconsistent");
//            if (expects == 1)
//                span.log(", contrary to expectation");
//            else if (expects == 0)
//                span.log(", as expected");
              span.append(". " + array[4] + "ms.\n");
//            span.logBold("   Minimizing the unsat core of " + array[3] + " entries...\n");
			}
			if (array[0].equals("unsat")) {
				boolean chk = Boolean.TRUE.equals(array[1]);
				int expects = (Integer) (array[2]);
				String formula = (String) (array[4]);
//            span.setLength(len3);
				alloyMsg.messageType = AlloyLSMessageType.RunResult;
				alloyMsg.replaceLast = true;
				alloyMsg.bold = true;
				span.append(chk ? "   No counterexample found. " : "   No instance found. ");
				span.append(chk ? "Assertion" : "Predicate");// , formula);
				span.append(chk ? " may be valid" : " may be inconsistent");
				if (expects == 1)
					span.append(", contrary to expectation");
				else if (expects == 0)
					span.append(", as expected");
				if (array.length == 5) {
					span.append(". ");
					alloyMsg.lineBreak = false;
					resMsgs.add(new AlloyLSMessage(RunResult, array[3] + "ms.\n\n"));
//                span.flush();
				} else {
					String core = (String) (array[5]);
					int mbefore = (Integer) (array[6]), mafter = (Integer) (array[7]);
					span.append(". " + array[3] + "ms.\n");
					if (core.length() == 0) {
						// results.add("");
						span.append("   No unsat core is available in this case. ");
						
						resMsgs.add(new AlloyLSMessage(RunResult, array[8] + "ms.\n\n"));
						// span.flush();
					} else {

						// results.add(core);
						(new File(core)).deleteOnExit();
						span.append("   ");
						span.append("Core");// , core);
						if (mbefore <= mafter)
							span.append(" contains " + mafter + " top-level formulas. " + array[8] + "ms.\n\n");
						else
							span.append(" reduced from " + mbefore + " to " + mafter + " top-level formulas. "
									+ array[8] + "ms.\n\n");
					}
				}
			}
		}
		alloyMsg.message = span.toString();
		return Either.forLeft(resMsgs);
	}

	   /**
     * This method stops the current run or check (how==0 means DONE, how==1 means
     * FAIL, how==2 means STOP).
     */
    void doStop(Integer how) {
        int h = how;
        if (h != 0) {
            if (h == 2 && WorkerEngine.isBusy()) {
                WorkerEngine.stop();
                //log.logBold("\nSolving Stopped.\n");
                //log.logDivider();
            }
            WorkerEngine.stop();
        }
//        runmenu.setEnabled(true);
//        runbutton.setVisible(true);
//        showbutton.setEnabled(true);
//        stopbutton.setVisible(false);
//        if (latestAutoInstance.length() > 0) {
//            String f = latestAutoInstance;
//            latestAutoInstance = "";
//            if (subrunningTask == 2)
//                viz.loadXML(f, true);
//            else if (AutoVisualize.get() || subrunningTask == 1)
//                doVisualize("XML: " + f);
//        }
    }

	// edu.mit.csail.sdg.alloy4whole.SimpleGUI.doVisualize(String) for handling
	// instance visualization links

	private void doVisualize(String arg) {
		if (arg.startsWith("CORE: ")) { // CORE: filename
			String filename = Util.canon(arg.substring(6));
			Pair<Set<Pos>, Set<Pos>> hCore;
			// Set<Pos> lCore;
			InputStream is = null;
			ObjectInputStream ois = null;
			try {
				is = new FileInputStream(filename);
				ois = new ObjectInputStream(is);
				hCore = (Pair<Set<Pos>, Set<Pos>>) ois.readObject();
				// lCore = (Set<Pos>) ois.readObject();
			} catch (Throwable ex) {
				System.err.println("Error reading or parsing the core \"" + filename + "\"\n");
				// return null;
			} finally {
				Util.close(ois);
				Util.close(is);
			}
		}
		if (arg.startsWith("POS: ")) { // POS: x1 y1 x2 y2 filename
			Scanner s = new Scanner(arg.substring(5));
			int x1 = s.nextInt(), y1 = s.nextInt(), x2 = s.nextInt(), y2 = s.nextInt();
			String f = s.nextLine();
			if (f.length() > 0 && f.charAt(0) == ' ')
				f = f.substring(1); // Get rid of the space after Y2
			Pos p = new Pos(Util.canon(f), x1, y1, x2, y2);

		}
		if (arg.startsWith("CNF: ")) { // CNF: filename
			String filename = Util.canon(arg.substring(5));
			try {
				String text = Util.readAll(filename);
				OurDialog.showtext("Text Viewer", text);
			} catch (IOException ex) {
				System.err.println("Error reading the file \"" + filename + "\"\n");
			}
		}
		if (arg.startsWith("XML: ")) { // XML: filename

			// from SimpleGui
			// VizGUI viz = new VizGUI(false, "", windowmenu2, enumerator, evaluator);
			VizGUI viz = new VizGUI(false, "", null, null, null);
			viz.loadXML(Util.canon(arg.substring(5)), false);
		}
	}
	public interface Func0<T> {
		public T call() throws Exception;
	}
	public static <T> T uncheckedRun(Func0<T> func) {
		try {
			return func.call();
		} catch(RuntimeException e) {
			throw e;
		} catch(Exception e) {
			throw new RuntimeException(e);
		}
	}

}
