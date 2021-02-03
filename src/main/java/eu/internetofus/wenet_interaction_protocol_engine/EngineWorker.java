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

import eu.internetofus.common.components.Model;
import eu.internetofus.common.components.incentive_server.Incentive;
import eu.internetofus.common.components.interaction_protocol_engine.ProtocolMessage;
import eu.internetofus.common.components.profile_manager.CommunityProfile;
import eu.internetofus.common.components.profile_manager.WeNetProfileManager;
import eu.internetofus.common.components.task_manager.Task;
import eu.internetofus.common.components.task_manager.TaskTransaction;
import eu.internetofus.common.components.task_manager.TaskType;
import eu.internetofus.common.components.task_manager.WeNetTaskManager;
import eu.internetofus.common.vertx.Worker;
import eu.internetofus.wenet_interaction_protocol_engine.persistence.Protocol;
import io.vertx.core.AbstractVerticle;
import io.vertx.core.Handler;
import io.vertx.core.Promise;
import io.vertx.core.eventbus.Message;
import io.vertx.core.eventbus.MessageConsumer;
import io.vertx.core.json.JsonObject;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import org.apache.commons.io.FileUtils;
import org.tinylog.Level;
import org.tinylog.Logger;
import org.tinylog.provider.InternalLogger;

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
  public static final String[] PROLOG_FILE_NAMES = { "common.pl", "profile_manager.pl", "task_manager.pl", "service.pl",
      "ontology.pl", "norms.pl", "engine.pl", "main.pl" };

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

      final var prologConf = this.config().getJsonObject("engine", new JsonObject()).getJsonObject("prolog",
          new JsonObject());
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
  public void handle(final Message<JsonObject> event) {

    try {

      final var body = event.body();
      final var type = MessageForWorkerBuilder.Type
          .valueOf(body.getString("type", MessageForWorkerBuilder.Type.DO_TASK_TRANSACTION.name()));
      switch (type) {
      case SEND_INCENTIVE:
        this.handleSendIncentive(body);
        break;
      case CREATED_TASK:
        this.handleCreatedTask(body);
        break;
      case DO_TASK_TRANSACTION:
        this.handleDoTaskTransaction(body);
        break;
      case PROTOCOL_MESSAGE:
        this.handleProtocolMessage(body);
        break;
      default:
        Logger.trace("Can not process the event {}, because does not contains a valid Message.", event);
      }

    } catch (final Throwable throwable) {

      Logger.trace(throwable, "Can not process the event {}", event);
    }

  }

  /**
   * Called when have received a send incentive message.
   *
   * @param message with the incentive.
   */
  protected void handleSendIncentive(final JsonObject message) {

    final int step = message.getInteger("step", 0);
    final var incentive = Model.fromJsonObject(message.getJsonObject("incentive"), Incentive.class);
    final var community = Model.fromJsonObject(message.getJsonObject("community"), CommunityProfile.class);

    this.vertx.eventBus().publish(EngineWorker.ADDRESSS, message);

  }

  /**
   * Called when a task has been created.
   *
   * @param message with the created task.
   */
  protected void handleCreatedTask(final JsonObject message) {

    final var task = Model.fromJsonObject(message.getJsonObject("task"), Task.class);
    final var taskType = Model.fromJsonObject(message.getJsonObject("taskType"), TaskType.class);

  }

  /**
   * Called when has to do a task transaction.
   *
   * @param message with the transaction to do.
   */
  protected void handleDoTaskTransaction(final JsonObject message) {

    final var transaction = Model.fromJsonObject(message.getJsonObject("transaction"), TaskTransaction.class);
    final var taskType = Model.fromJsonObject(message.getJsonObject("taskType"), TaskType.class);

  }

  /**
   * Called when has to process a message of the protocol.
   *
   * @param message with the protocol message.
   */
  protected void handleProtocolMessage(final JsonObject message) {

    final var protocolMessage = Model.fromJsonObject(message.getJsonObject("message"), ProtocolMessage.class);
    if (protocolMessage.taskId != null && !message.containsKey("task")) {

      WeNetTaskManager.createProxy(this.vertx).retrieveTask(protocolMessage.taskId).onComplete(retrieveTask -> {

        final var task = retrieveTask.result();
        if (task == null) {

          Logger.warn(retrieveTask.cause(), "Cannot import the task for {}", message);
          message.putNull("task");

        } else {

          message.put("task", task.toJsonObject());
        }
        WeNetTaskManager.createProxy(this.vertx).retrieveTaskType(task.taskTypeId).onComplete(retrieveTaskType -> {

          final var taskType = retrieveTaskType.result();
          if (taskType == null) {

            Logger.warn(retrieveTaskType.cause(), "Cannot import the task type for {}", message);
            message.putNull("taskType");

          } else {

            message.put("taskType", taskType.toJsonObject());
          }

          this.vertx.eventBus().publish(EngineWorker.ADDRESSS, message);
        });

      });

    } else if (protocolMessage.communityId != null && !message.containsKey("community")) {

      WeNetProfileManager.createProxy(this.vertx).retrieveCommunity(protocolMessage.communityId)
          .onComplete(retrieve -> {

            final var community = retrieve.result();
            if (community == null) {

              Logger.warn(retrieve.cause(), "Cannot import the community for {}", message);
              message.putNull("community");

            } else {

              message.put("community", community.toJsonObject());
            }
            this.vertx.eventBus().publish(EngineWorker.ADDRESSS, message);

          });

    } else {

      final var task = Model.fromJsonObject(message.getJsonObject("task"), Task.class);
      final var taskType = Model.fromJsonObject(message.getJsonObject("taskType"), TaskType.class);
      final var community = Model.fromJsonObject(message.getJsonObject("community"), CommunityProfile.class);

    }
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
      final var index = init.length();
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

      final var processBuilder = new ProcessBuilder("swipl", "--debug", "-O", "-s",
          initPath.toAbsolutePath().toString(), "-g", "go", "-t", "halt");
      processBuilder.directory(tmp.toFile());
      processBuilder.redirectError(error.toFile());
      processBuilder.redirectOutput(output.toFile());
      final var process = processBuilder.start();
      Logger.trace("Start process with {}", () -> processBuilder.command(), () -> process.pid());
      final var status = process.waitFor();
      InternalLogger.log(Level.ERROR, readQuietly(error));
      InternalLogger.log(Level.DEBUG, readQuietly(output));
      if (status == 0) {

        Logger.trace("Finished process {} successfully.\nThe error stream is:\n{}\nThe output stream is:\n{}",
            () -> process.pid(), () -> readQuietly(error), () -> readQuietly(output));

      } else {

        Logger.error("Finished process {} with error code {}.\nThe error stream is:\n{}\nThe output stream is:\n{}",
            () -> process.pid(), () -> status, () -> readQuietly(error), () -> readQuietly(output));
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
