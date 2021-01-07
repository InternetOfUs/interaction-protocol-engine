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

import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;

import org.apache.commons.io.FileUtils;
import org.tinylog.Level;
import org.tinylog.Logger;
import org.tinylog.provider.InternalLogger;

import eu.internetofus.common.components.Model;
import eu.internetofus.common.components.interaction_protocol_engine.ProtocolMessage;
import eu.internetofus.common.vertx.Worker;
import eu.internetofus.wenet_interaction_protocol_engine.persistence.Protocol;
import io.vertx.core.AbstractVerticle;
import io.vertx.core.Handler;
import io.vertx.core.Promise;
import io.vertx.core.eventbus.MessageConsumer;
import io.vertx.core.json.JsonObject;

/**
 * The worker verticle that is used to process the messages for an interaction protocol.
 *
 * @author UDT-IA, IIIA-CSIC
 */
@Worker
public class EngineWorker extends AbstractVerticle implements Handler<io.vertx.core.eventbus.Message<JsonObject>> {

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
  public static final String[] PROLOG_FILE_NAMES = { "common.pl", "profile_manager.pl", "task_manager.pl", "service.pl", "ontology.pl", "norms.pl", "engine.pl", "main.pl" };

  /**
   * The type that define the the received event of the type incentive.
   */
  public static final String SEND_INCENTIVE_TYPE = "send_incentive";

  /**
   * The type that define the the received event of the type incentive.
   */
  public static final String CREATED_TASK = "created_task";

  /**
   * The type that define the the received event of the type task transaction.
   */
  public static final String DO_TASK_TRANSACTION = "do_transaction";

  /**
   * The component that will consume the messages.
   */
  protected MessageConsumer<JsonObject> consumer;

  /**
   * The file where the prolog engine is stored.
   */
  protected Path prologDir;

  /**
   * {@inheritDoc}
   */
  @Override
  public void start(final Promise<Void> startPromise) throws Exception {

    this.consumer = this.vertx.eventBus().consumer(ADDRESSS, this);
    this.consumer.completionHandler(completion -> {

      if (completion.failed()) {

        startPromise.fail(completion.cause());

      } else {

        this.vertx.executeBlocking(this::copyResources, res -> startPromise.handle(res));

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

      final var prologConf = this.config().getJsonObject("engine", new JsonObject()).getJsonObject("prolog", new JsonObject());
      this.prologDir = Paths.get(prologConf.getString("workDir", "var/prolog"));
      this.prologDir.toFile().mkdirs();
      for (final String prologFileName : PROLOG_FILE_NAMES) {

        final var resource = PROLOG_RESOURCE_DIR + prologFileName;
        final var engineStream = this.getClass().getClassLoader().getResourceAsStream(resource);
        if (engineStream == null) {

          Logger.error("Not found {}", resource);
          promise.fail("Not found " + resource);
          return;

        } else {

          final var file = this.prologDir.resolve(Paths.get(prologFileName));
          Files.copy(engineStream, file, StandardCopyOption.REPLACE_EXISTING);

        }

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
  public void handle(final io.vertx.core.eventbus.Message<JsonObject> event) {

    try {

      final var body = event.body();
      final var type = MessageForWorkerBuilder.Type.valueOf(body.getString("type", MessageForWorkerBuilder.Type.DO_TASK_TRANSACTION.name()));
      if (SEND_INCENTIVE_TYPE.equals(type)) {
        // send incentive

      } else {
        final var message = Model.fromJsonObject(body, ProtocolMessage.class);
        if (message == null) {

          Logger.trace("Can not process the event {}, because does not contains a valid Message.", event);

        } else {

          this.handle(message);
        }

      }

    } catch (final Throwable throwable) {

      Logger.trace(throwable, "Can not process the event {}", event);
    }

  }

  /**
   * Called when have a message to process.
   *
   * @param message to process.
   */
  protected void handle(final ProtocolMessage message) {

    Logger.trace("Received message to process {}", message);

    // final var params = new ModelsPageContext();
    // params.sort = ProtocolsRepository.createProtocolsPageQuery(message.appId, message.communityId, message.taskTypeId,
    // message.taskId);
    // params.offset = 0;
    // params.limit = 1;
    // ProtocolsRepository.createProxy(this.vertx).retrieveProtocolsPage(params, search -> {
    //
    // final var page = search.result();
    // final var protocol = page.protocols.get(0);
    // this.handleSWIProlog(message, protocol);
    //
    // });

  }

  /**
   * Handle a SWIProlog event.
   *
   * @param message  that contains the task transaction.
   * @param protocol to check.
   */
  protected void handleSWIProlog(final ProtocolMessage message, final Protocol protocol) {

    Path tmp = null;
    try {

      tmp = Files.createTempDirectory("engine_worker");
      final var error = tmp.resolve(Paths.get("error.txt"));
      final var output = tmp.resolve(Paths.get("output.txt"));

      final var messagePath = tmp.resolve(Paths.get("message.json"));
      Files.writeString(messagePath, message.toJsonString());
      final var init = new StringBuilder();
      init.append("wenet_message_file('");
      init.append(messagePath.toAbsolutePath());
      init.append("').\n");

      final var configurationPath = tmp.resolve(Paths.get("configuration.json"));
      Files.writeString(configurationPath, this.config().encode());
      init.append("wenet_configuration_file('");
      init.append(configurationPath.toAbsolutePath());
      init.append("').\n");

      // Add to the init the files to load
      init.append(":- load_files([");
      final int index = init.length();
      Files.list(this.prologDir).filter(path -> path.getFileName().toString().endsWith(".pl")).forEach(path -> {

        init.append(",\n\t'");
        init.append(path.toAbsolutePath());
        init.append("'");

      });
      // At this point init ends with a ,
      init.deleteCharAt(index);// Remove the extra ,

      if (protocol.ontology != null) {
        // Load the protocol ontology
        final var protocolOntologyPath = tmp.resolve(Paths.get("protocol_ontology.json"));
        Files.writeString(protocolOntologyPath, protocol.ontology);
        init.append(",\n\t'");
        init.append(protocolOntologyPath.toAbsolutePath());
        init.append("'");
      }

      if (protocol.norms != null) {
        // Load the protocol norms
        final var protocolNormsPath = tmp.resolve(Paths.get("protocol_norms.json"));
        Files.writeString(protocolNormsPath, protocol.norms);
        init.append(",\n\t'");
        init.append(protocolNormsPath.toAbsolutePath());
        init.append("'");
      }

      init.append("\n\t],[autoload(true)]).\n");

      InternalLogger.log(Level.ERROR, init.toString());

      final var initPath = tmp.resolve(Paths.get("init.pl"));
      Files.writeString(initPath, init.toString());

      final var processBuilder = new ProcessBuilder("swipl", "--debug", "-O", "-s", initPath.toAbsolutePath().toString(), "-g", "go", "-t", "halt");
      processBuilder.directory(tmp.toFile());
      processBuilder.redirectError(error.toFile());
      processBuilder.redirectOutput(output.toFile());
      final var process = processBuilder.start();
      Logger.trace("Start process with {}", () -> processBuilder.command(), () -> process.pid());
      final var status = process.waitFor();
      InternalLogger.log(Level.ERROR, readQuietly(error));
      InternalLogger.log(Level.DEBUG, readQuietly(output));
      if (status == 0) {

        Logger.trace("Finished process {} successfully.\nThe error stream is:\n{}\nThe output stream is:\n{}", () -> process.pid(), () -> readQuietly(error), () -> readQuietly(output));

      } else {

        Logger.error("Finished process {} with error code {}.\nThe error stream is:\n{}\nThe output stream is:\n{}", () -> process.pid(), () -> status, () -> readQuietly(error), () -> readQuietly(output));
      }

    } catch (final Throwable processError) {

      Logger.error(processError, "Cannot process the SWI Prolog message");
    }

    if (!FileUtils.deleteQuietly(tmp.toFile())) {

      Logger.error("Cannot remove the working directory {}", tmp);

    }

  }

  /**
   * Read a file as string capturing any exception.
   *
   * @param path to read.
   *
   * @return the content of the file, or the error if can not read it.
   */
  private static String readQuietly(final Path path) {

    try {

      return Files.readString(path, Charset.defaultCharset());

    } catch (final Throwable error) {

      return error.toString();
    }

  }

}
