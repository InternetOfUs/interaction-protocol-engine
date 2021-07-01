/*
 * -----------------------------------------------------------------------------
 *
 * Copyright (c) 2019 - 2022 UDT-IA, IIIA-CSIC
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 * -----------------------------------------------------------------------------
 */

package eu.internetofus.wenet_interaction_protocol_engine;

import eu.internetofus.common.components.incentive_server.WeNetIncentiveServerClient;
import eu.internetofus.common.components.interaction_protocol_engine.WeNetInteractionProtocolEngine;
import eu.internetofus.common.components.models.ProtocolNorm;
import eu.internetofus.common.components.personal_context_builder.WeNetPersonalContextBuilderClient;
import eu.internetofus.common.components.profile_manager.WeNetProfileManagerClient;
import eu.internetofus.common.components.service.WeNetServiceClient;
import eu.internetofus.common.components.social_context_builder.WeNetSocialContextBuilderClient;
import eu.internetofus.common.components.task_manager.WeNetTaskManagerClient;
import eu.internetofus.common.model.Model;
import eu.internetofus.common.model.TimeManager;
import eu.internetofus.common.vertx.AbstractServicesVerticle;
import eu.internetofus.common.vertx.Worker;
import io.vertx.core.AbstractVerticle;
import io.vertx.core.Handler;
import io.vertx.core.Promise;
import io.vertx.core.eventbus.Message;
import io.vertx.core.eventbus.MessageConsumer;
import io.vertx.core.json.JsonObject;
import java.io.Closeable;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;
import java.util.function.BiConsumer;
import java.util.function.BooleanSupplier;
import org.apache.commons.io.FileUtils;
import org.tinylog.Logger;

/**
 * The worker verticle that is used to process the messages for an interaction
 * protocol.
 *
 * @author UDT-IA, IIIA-CSIC
 */
@Worker
public class EngineWorker extends AbstractVerticle implements Handler<Message<JsonObject>> {

  /**
   * The address used to send messages to the worker.
   */
  public static final String ADDRESSS = "eu.internetofus.wenet_interaction_protocol_engine.engine_worker";

  /**
   * The path to the resource with the engine file.
   */
  public static final String PROLOG_RESOURCE_DIR = "eu/internetofus/wenet_interaction_protocol_engine/";

  /**
   * The name of the files of the prolog files to copy.
   */
  public static final String[] PROLOG_FILE_NAMES = { "common.pl", "profile_manager.pl", "task_manager.pl",
      "interaction_protocol_engine.pl", "social_context_builder.pl", "service.pl", "incentive_server.pl",
      "personal_context_builder.pl", "conditions.pl", "actions.pl", "engine.pl", "main.pl", "ontology.pl", "norms.pl" };

  /**
   * The component that will consume the messages.
   */
  protected MessageConsumer<JsonObject> consumer;

  /**
   * The file where the prolog engine is stored.
   */
  protected Path prologDir;

  /**
   * The URL to the interaction protocol engine API.
   */
  protected String interactionProtocolEngineApi;

  /**
   * {@inheritDoc}
   */
  @Override
  public void start(final Promise<Void> startPromise) throws Exception {

    WeNetInteractionProtocolEngine.createProxy(this.vertx).obtainApiUrl().onComplete(getter -> {

      if (getter.failed()) {

        startPromise.fail(getter.cause());

      } else {

        this.interactionProtocolEngineApi = getter.result();
        this.consumer = this.vertx.eventBus().consumer(ADDRESSS, this);
        this.consumer.completionHandler(completion -> {

          if (completion.failed()) {

            startPromise.fail(completion.cause());

          } else {

            this.vertx.executeBlocking(this::copyResources, res -> startPromise.handle(res));

          }

        });

      }

    });
  }

  /**
   * Copy all the resource with the prolog files.
   *
   * @param promise to inform of the load result.
   */
  protected void copyResources(final Promise<Void> promise) {

    try {

      final var prologConf = this.config().getJsonObject("engine", new JsonObject()).getJsonObject("prolog",
          new JsonObject());
      this.prologDir = Paths.get(prologConf.getString("workDir", "var/prolog"));
      this.prologDir.toFile().mkdirs();
      var index = 0;
      for (final String prologFileName : PROLOG_FILE_NAMES) {

        final var resource = PROLOG_RESOURCE_DIR + prologFileName;
        final var engineStream = this.getClass().getClassLoader().getResourceAsStream(resource);
        if (engineStream == null) {

          Logger.error("Not found {}", resource);
          promise.fail("Not found " + resource);
          return;

        } else {

          final var newFileName = String.format("%04d", index) + "_" + prologFileName;
          final var file = this.prologDir.resolve(Paths.get(newFileName));
          Files.copy(engineStream, file, StandardCopyOption.REPLACE_EXISTING);

        }
        index++;

      }
      promise.complete();

    } catch (final Throwable cause) {

      promise.fail(cause);
    }

  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void handle(final Message<JsonObject> event) {

    try (var env = new SWIProplogEnvironment()) {

      final var body = event.body();
      final var protocol = Model.fromJsonObject(body.getJsonObject("protocol"), ProtocolData.class);

      env.fillInAutoloadPrologFilesIn(this.prologDir);
      env.appendToInitConfigurationFacts(this.config());
      env.fillIn(protocol);
      final var message = body.getJsonObject("message");
      env.appendToInitAssertaModel(message, "message");
      env.include(env.protocolOntology);
      env.include(env.protocolNorms);

      env.run(message);

    } catch (final Throwable throwable) {

      Logger.trace(throwable, "Can not process the event {}", event);
    }

  }

  /**
   * The environment to execute the SWIProlog.
   */
  protected class SWIProplogEnvironment implements Closeable {

    /**
     * The working directory.
     */
    protected Path work;

    /**
     * The path to the file with the protocol norms.
     */
    protected Path protocolNorms;

    /**
     * The path to the file with the protocol ontology.
     */
    protected Path protocolOntology;

    /**
     * The path to the init file.
     */
    protected Path init;

    /**
     * Create the environment to execute the SWIProlog.
     *
     * @throws IOException If can not create a necessary file.
     */
    public SWIProplogEnvironment() throws IOException {

      this.work = Files.createTempDirectory("engine_worker");
      this.init = this.createFileAtWork("init.pl");
      Files.writeString(this.init, "%\n% Initialize norm engine\n%\n\n");
      this.protocolNorms = this.createFileAtWork("protocol_norms.pl");
      this.protocolOntology = this.createFileAtWork("protocol_ontology.pl");

    }

    /**
     * Create the file with the specified name on the work.
     *
     * @param name of the file.
     *
     * @return the created file.
     *
     * @throws IOException If can not create the new file.
     */
    protected Path createFileAtWork(final String name) throws IOException {

      final var path = this.work.resolve(Paths.get(name));
      path.toFile().createNewFile();
      return path;

    }

    /**
     * Add to the init file to load all the files.
     *
     * @param dir directory to obtain the files to autoload.
     *
     * @throws IOException If can not obtain the files to load.
     */
    public void fillInAutoloadPrologFilesIn(final Path dir) throws IOException {

      final var iter = Files.list(dir).filter(path -> path.getFileName().toString().endsWith(".pl")).sorted()
          .iterator();
      while (iter.hasNext()) {

        final var path = iter.next();
        this.include(path);

      }

    }

    /**
     * Include a file into the init file.
     *
     * @param path to the file to include.
     *
     * @throws IOException If can not include the file to load.
     */
    public void include(final Path path) throws IOException {

      final var content = new StringBuilder();
      content.append(":- include('");
      content.append(path.toAbsolutePath());
      content.append("').\n");
      Files.writeString(this.init, content, StandardOpenOption.APPEND);

    }

    /**
     * Add to the init file the path to a file to load the data defined by the
     * prefix.
     *
     * @param model  to load.
     * @param prefix that define the data.
     *
     * @throws IOException If can not append to component to load the model.
     */
    public void appendToInitAssertaModel(final JsonObject model, final String prefix) throws IOException {

      final var content = new StringBuilder();
      final var modelFile = this.createFileAtWork("wenet_protocol_" + prefix + ".json");
      Files.writeString(modelFile, model.encode());

      this.appendFact(content, "wenet_protocol_" + prefix + "_file", modelFile.toAbsolutePath().toString());
      Files.writeString(this.init, content, StandardOpenOption.APPEND);

    }

    /**
     * Fill in the data of the protocol.
     *
     * @param protocol to extract the data to fill in.
     *
     * @throws IOException If can not fill in the data form the protocol.
     */
    public void fillIn(final ProtocolData protocol) throws IOException {

      if (protocol.profile != null) {

        this.appendToInitAssertaModel(protocol.profile.toJsonObject(), "profile");
        this.appendNorms(protocol.profile.norms, "Norms of profile ", protocol.profile.id);
      }

      if (protocol.community != null) {

        this.appendToInitAssertaModel(protocol.community.toJsonObject(), "community");
        this.appendNorms(protocol.community.norms, "Norms of community ", protocol.community.name, " (",
            protocol.community.id, ")");

      }

      if (protocol.taskType != null) {

        this.appendToInitAssertaModel(protocol.taskType.toJsonObject(), "task_type");
        this.appendNorms(protocol.taskType.norms, "Norms of task type ", protocol.taskType.name, " (",
            protocol.taskType.id, ")");

      }

      if (protocol.task != null) {

        this.appendToInitAssertaModel(protocol.task.toJsonObject(), "task");
        this.appendNorms(protocol.task.norms, "Norms of task  ", protocol.task.id);

      }

    }

    /**
     * Append the norms into the protocol norms file.
     *
     * @param norms   to fill in.
     * @param headers for the norms.
     *
     * @throws IOException If can not write into the protocol norms.
     */
    protected void appendNorms(final Iterable<ProtocolNorm> norms, final String... headers) throws IOException {

      final var ontologyContent = new StringBuilder();
      final var normsContent = new StringBuilder();
      normsContent.append("\n% ");
      for (final var header : headers) {

        normsContent.append(header);
      }
      normsContent.append("\n");

      if (norms != null) {

        for (final var norm : norms) {

          if (norm.ontology != null) {

            ontologyContent.append("\n%\n% Ontology of norm:\n%\twhenever\n%\t\t");
            ontologyContent.append(norm.whenever.trim());
            ontologyContent.append("\n%\tthenceforth\n%\t\t");
            ontologyContent.append(norm.thenceforth.trim());
            ontologyContent.append("\n%\n");
            ontologyContent.append(norm.ontology.trim());
            ontologyContent.append("\n");
          }

          normsContent.append("\nwhenever\n\t");
          normsContent.append(norm.whenever.trim().replaceAll("\\sand\\s", "\n\tand "));
          normsContent.append("\nthenceforth\n\t");
          normsContent.append(norm.thenceforth.trim().replaceAll("\\sand\\s", "\n\tand "));
          normsContent.append(".\n");
        }
      }

      Files.writeString(this.protocolOntology, ontologyContent, StandardOpenOption.APPEND);
      Files.writeString(this.protocolNorms, normsContent, StandardOpenOption.APPEND);

    }

    /**
     * Append to the initialization file the configuration facts.
     *
     * @param config configuration of the component.
     *
     * @throws IOException If can not append the content to the init file.
     */
    public void appendToInitConfigurationFacts(final JsonObject config) throws IOException {

      final var content = new StringBuilder();
      final var components = config.getJsonObject("wenetComponents", new JsonObject());

      content.append("get_now(");
      content.append(TimeManager.now());
      content.append(").\n");

      this.appendFact(content, "wenet_profile_manager_api_url",
          components.getString(WeNetProfileManagerClient.PROFILE_MANAGER_CONF_KEY,
              WeNetProfileManagerClient.DEFAULT_PROFILE_MANAGER_API_URL));

      this.appendFact(content, "wenet_task_manager_api_url", components.getString(
          WeNetTaskManagerClient.TASK_MANAGER_CONF_KEY, WeNetTaskManagerClient.DEFAULT_TASK_MANAGER_API_URL));

      this.appendFact(content, "wenet_interaction_protocol_engine_api_url",
          EngineWorker.this.interactionProtocolEngineApi);

      this.appendFact(content, "wenet_social_context_builder_api_url",
          components.getString(WeNetSocialContextBuilderClient.SOCIAL_CONTEXT_BUILDER_CONF_KEY,
              WeNetSocialContextBuilderClient.DEFAULT_SOCIAL_CONTEXT_BUILDER_API_URL));

      this.appendFact(content, "wenet_service_api_url",
          components.getString(WeNetServiceClient.SERVICE_CONF_KEY, WeNetServiceClient.DEFAULT_SERVICE_API_URL));

      this.appendFact(content, "wenet_incentive_server_api_url",
          components.getString(WeNetIncentiveServerClient.INCENTIVE_SERVER_CONF_KEY,
              WeNetIncentiveServerClient.DEFAULT_INCENTIVE_SERVER_API_URL));

      this.appendFact(content, "wenet_personal_context_builder_api_url",
          components.getString(WeNetPersonalContextBuilderClient.PERSONAL_CONTEXT_BUILDER_CONF_KEY,
              WeNetPersonalContextBuilderClient.DEFAULT_PERSONAL_CONTEXT_BUILDER_API_URL));

      content.append("wenet_component_auth_header(request_header('");
      content.append(AbstractServicesVerticle.WENET_COMPONENT_APIKEY_HEADER);
      content.append("' = '");
      content.append(config.getJsonObject(AbstractServicesVerticle.WEB_CLIENT_CONF_KEY, new JsonObject())
          .getString(AbstractServicesVerticle.WENET_COMPONENT_APIKEY_CONF_KEY, "UNDEFINED"));
      content.append("')).\n");

      Files.writeString(this.init, content, StandardOpenOption.APPEND);

    }

    /**
     * Append a fact.
     *
     * @param content to append the fact.
     * @param key     of the fact.
     * @param value   of the fact.
     */
    protected void appendFact(final StringBuilder content, final String key, final String value) {

      content.append(key);
      content.append("('");
      content.append(value);
      content.append("').\n");

    }

    /**
     * Run the.
     *
     * @param message to process.
     *
     * @throws Throwable If cannot execute the SWIProlog
     */
    public void run(final JsonObject message) throws Throwable {

      final var processBuilder = new ProcessBuilder("swipl", "--debug", "-O", "-s",
          this.init.toAbsolutePath().toString(), "-g", "go", "-t", "halt");
      processBuilder.directory(this.work.toFile());
      final var error = this.work.resolve(Paths.get("error.txt"));
      final var output = this.work.resolve(Paths.get("output.txt"));
      processBuilder.redirectError(error.toFile());
      processBuilder.redirectOutput(output.toFile());
      final var process = processBuilder.start();
      final var pid = process.pid();
      Logger.trace("Start swipl process {} for {}", pid, message);
      final var status = process.waitFor();
      if (status == 0) {

        Logger.trace("Finished swipl process {} successfully.", pid);

      } else {

        Logger.error("Finished swipl process {} with error code {}.", pid, status);
      }

      EngineWorker.this.logLinesOf(error, Logger::isErrorEnabled, "ERROR:", Logger::error, pid);
      EngineWorker.this.logLinesOf(output, Logger::isTraceEnabled, "TRACE:", Logger::trace, pid);

    }

    /**
     * Remove completely the working directory.
     *
     * {@inheritDoc}
     */
    @Override
    public void close() throws IOException {

      if (!FileUtils.deleteQuietly(this.work.toFile())) {

        Logger.error("Cannot remove the working directory {}", this.work);

      }

    }

  }

  /**
   * Read the lines of a file and consume as log messages.
   *
   * @param path       to read.
   * @param logEnabled return {@code true} if the log is enabled.
   * @param prefix     for the log lines.
   * @param logger     the function to append the log.
   * @param pid        identifier of the swipl process.
   */
  private void logLinesOf(final Path path, final BooleanSupplier logEnabled, final String prefix,
      final BiConsumer<String, Object[]> logger, final long pid) {

    if (logEnabled.getAsBoolean()) {

      try {

        final var content = Files.readString(path, Charset.defaultCharset());
        final var logMessages = content.split(prefix);
        for (final var logMessage : logMessages) {

          final var msg = logMessage.trim();
          if (msg.length() > 0) {

            logger.accept("swipl [{}] {}", new Object[] { pid, msg });
          }

        }

      } catch (final Throwable error) {

        Logger.error(error, "Cannot append logs of {}", path);
      }

    }

  }

}
