
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

import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;
import java.nio.charset.Charset;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.tinylog.Logger;

import com.google.common.io.Files;

import eu.internetofus.common.TimeManager;
import eu.internetofus.common.components.Model;
import eu.internetofus.common.components.incentive_server.Incentive;
import eu.internetofus.common.components.incentive_server.TaskStatus;
import eu.internetofus.common.components.incentive_server.WeNetIncentiveServer;
import eu.internetofus.common.components.interaction_protocol_engine.Message;
import eu.internetofus.common.components.service.App;
import eu.internetofus.common.components.service.TaskConcludedNotification;
import eu.internetofus.common.components.service.TaskProposalNotification;
import eu.internetofus.common.components.service.TaskSelectionNotification;
import eu.internetofus.common.components.service.TaskVolunteerNotification;
import eu.internetofus.common.components.service.TextualMessage;
import eu.internetofus.common.components.service.WeNetService;
import eu.internetofus.common.components.social_context_builder.WeNetSocialContextBuilder;
import eu.internetofus.common.components.task_manager.Task;
import eu.internetofus.common.components.task_manager.TaskTransaction;
import eu.internetofus.common.components.task_manager.WeNetTaskManager;
import eu.internetofus.common.vertx.Worker;
import io.vertx.core.AbstractVerticle;
import io.vertx.core.Handler;
import io.vertx.core.Promise;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.eventbus.MessageConsumer;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.client.WebClient;
import io.vertx.ext.web.client.WebClientOptions;

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
  public static final String ADDRESSS = "eu.internetofus.wenet_interaction_protocol_engine.worker";

  /**
   * The path to the resource with the engine file.
   */
  public static final String ENGINE_RESOURCE = "eu/internetofus/wenet_interaction_protocol_engine/engine.pl";

  /**
   * The path to the resource with the engine file.
   */
  public static final String NORM_INTERPRETER_RESOURCE = "eu/internetofus/wenet_interaction_protocol_engine/Norm_interpreter.pl";

  /**
   * The path to the resource with the engine file.
   */
  public static final String HARDCODED_NORMS_RESOURCE = "eu/internetofus/wenet_interaction_protocol_engine/WeNet_ChatBot_norms.pl";

  /**
   * The component that will consume the messages.
   */
  protected MessageConsumer<JsonObject> consumer;

  /**
   * The file where the prolog engine is stored.
   */
  protected File engine;

  /**
   * The file where the norm interpreter is stored.
   */
  protected File normInterpreter;

  /**
   * The file where are the hardcoded norms.
   */
  protected File communityNorms;

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
      final var workDir = new File(prologConf.getString("workDir", "var/prolog"));
      workDir.mkdirs();
      this.engine = this.copyResource(prologConf, workDir, "engineName", "engine.pl", ENGINE_RESOURCE);
      if (this.engine == null) {

        promise.fail("Not found engine file");

      } else {

        this.normInterpreter = this.copyResource(prologConf, workDir, "normInterpreter", "Norm_Interpreter.pl", NORM_INTERPRETER_RESOURCE);

        if (this.engine == null) {

          promise.fail("Not found norm interpreter");

        } else {

          this.communityNorms = this.copyResource(prologConf, workDir, "harcodedNorms", "WeNet_ChatBot_norms.pl", HARDCODED_NORMS_RESOURCE);
          if (this.engine == null) {

            promise.fail("Not found harcoded norms");

          } else {
            promise.complete();

          }

        }
      }

    } catch (final Throwable cause) {

      promise.fail(cause);
    }

  }

  /**
   * Copy a resource into a file.
   *
   * @param prologConf      configuration of the prolog.
   * @param workDir         directory to store the resources.
   * @param key             of the configuration property.
   * @param defaultFileName default name to the file to store the resource.
   * @param resource        path of the resource to copy.
   *
   * @return the file where is stored the resource or {@code null} if cannot load it.
   *
   * @throws IOException If the resource can not be copied.
   */
  private File copyResource(final JsonObject prologConf, final File workDir, final String key, final String defaultFileName, final String resource) throws IOException {

    final var engineStream = this.getClass().getClassLoader().getResourceAsStream(resource);
    if (engineStream == null) {

      return null;

    } else {

      final var file = new File(workDir, prologConf.getString(key, defaultFileName));
      final Writer writer = new FileWriter(file);
      IOUtils.copy(engineStream, writer);
      writer.close();
      return file;
    }

  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void handle(final io.vertx.core.eventbus.Message<JsonObject> event) {

    try {

      final var body = event.body();
      final var message = Model.fromJsonObject(body, Message.class);
      if (message == null) {

        Logger.trace("Can not process the event {}, because does not contains a valid Message.", event);

      } else {

        this.handle(message);
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
  protected void handle(final Message message) {

    Logger.trace("Received message to process {}", message);
    EngineEnvironment.create(this.vertx, message).onComplete(creation -> {

      try {

        final var env = creation.result();
        switch (message.type) {
        case TASK_CREATED:

          this.handleTaskCreated(env);
          break;
        case TASK_TRANSACTION:

          this.handleTaskTransaction(env, message);
          break;
        case INCENTIVE:

          this.handleIncentive(env, message);
          break;

        case SWI_PROLOG:

          this.handleSWIProlog(env, message);
          break;

        default:

          Logger.trace("Unexpected {}", message);
        }

      } catch (final Throwable throwable) {

        Logger.trace(throwable, "Can not process {}", message);
      }

    });

  }

  /**
   * Handle a task transaction.
   *
   * @param env     environment to use for the message.
   * @param message that contains the task transaction.
   */
  protected void handleTaskTransaction(final EngineEnvironment env, final Message message) {

    final var content = (JsonObject) message.content;
    final var options = new WebClientOptions();
    final var client = WebClient.create(this.vertx, options);
    final var transaction = Model.fromJsonObject(content, TaskTransaction.class);
    this.fixTaskAttributes(env, message);
    if (env.task.closeTs == null) {

      if ("volunteerForTask".equalsIgnoreCase(transaction.label)) {

        final var volunteerId = content.getJsonObject("attributes", new JsonObject()).getString("volunteerId", "0");
        this.handleVolunteerForTask(volunteerId, env, client);

      } else if ("refuseTask".equalsIgnoreCase(transaction.label)) {

        final var volunteerId = content.getJsonObject("attributes", new JsonObject()).getString("volunteerId", "0");
        this.handleRefuseTask(volunteerId, env, client);

      } else if ("acceptVolunteer".equalsIgnoreCase(transaction.label)) {

        final var volunteerId = content.getJsonObject("attributes", new JsonObject()).getString("volunteerId", "0");
        this.handleAcceptVolunteer(volunteerId, env, client);

      } else if ("refuseVolunteer".equalsIgnoreCase(transaction.label)) {

        final var volunteerId = content.getJsonObject("attributes", new JsonObject()).getString("volunteerId", "0");
        this.handleRefuseVolunteer(volunteerId, env, client);

      } else if ("taskCompleted".equalsIgnoreCase(transaction.label)) {

        final var outcome = content.getJsonObject("attributes", new JsonObject()).getString("outcome", "cancelled");
        this.handleTaskCompleted(outcome, env, client);

      } else {

        Logger.trace("Unexpected action {}", message);
      }

    } else {
      // Error task closed
      final var msg = new TextualMessage();
      msg.recipientId = transaction.attributes.getString("volunteerId", env.task.requesterId);
      msg.title = "Task already closed";
      msg.text = "It's too late the task is already completed.";
      this.sendTo(env.app, client, msg);
    }

  }

  /**
   * Called when want to send some messages into an application.
   *
   * @param app          application to notify.
   * @param client       to use.
   * @param notification to send to the application.
   */
  protected void sendTo(final App app, final WebClient client, final eu.internetofus.common.components.service.Message notification) {

    final var body = notification.toJsonObject();
    client.postAbs(app.messageCallbackUrl).sendJsonObject(body, send -> {

      if (send.failed()) {

        Logger.trace(send.cause(), "App[{}]: POST {} with {} failed", () -> app.appId, () -> app.messageCallbackUrl, () -> body);

      } else {

        final var response = send.result();
        Logger.trace("App[{}]: POST {} with {} responds with code {} and body {}", () -> app.appId, () -> app.messageCallbackUrl, () -> body, () -> response.statusCode(), () -> response.bodyAsString());
      }

    });

  }

  /**
   * Called when want to send a notification to multiple users of an application.
   *
   * @param users        to send the message.
   * @param app          application to notify.
   * @param client       to use.
   * @param notification to send to the application.
   */
  protected void sendTo(final JsonArray users, final App app, final WebClient client, final eu.internetofus.common.components.service.Message notification) {

    for (var i = 0; i < users.size(); i++) {

      notification.recipientId = users.getString(i);
      this.sendTo(app, client, notification);
    }

  }

  /**
   * Fix task attributes.
   *
   * @param env     environment where the message is processed.
   * @param message received about the task.
   */
  protected void fixTaskAttributes(final EngineEnvironment env, final Message message) {

    if (env.task == null) {

      env.task = new Task();
      env.task.id = message.taskId;

    }

    if (env.task.attributes == null) {

      env.task.attributes = new JsonObject();
    }

    final var unanswered = env.task.attributes.getJsonArray("unanswered", null);
    if (unanswered == null) {

      env.task.attributes.put("unanswered", new JsonArray());
    }
    final var volunteers = env.task.attributes.getJsonArray("volunteers", null);
    if (volunteers == null) {

      env.task.attributes.put("volunteers", new JsonArray());
    }
    final var declined = env.task.attributes.getJsonArray("declined", null);
    if (declined == null) {

      env.task.attributes.put("declined", new JsonArray());
    }
    final var accepted = env.task.attributes.getJsonArray("accepted", null);
    if (accepted == null) {

      env.task.attributes.put("accepted", new JsonArray());
    }
    final var refused = env.task.attributes.getJsonArray("refused", null);
    if (refused == null) {

      env.task.attributes.put("refused", new JsonArray());
    }

  }

  /**
   * Handle the message that inform that a task has been created.
   *
   * @param env to get the data.
   */
  private void handleTaskCreated(final EngineEnvironment env) {

    final var options = new WebClientOptions();
    final var client = WebClient.create(this.vertx, options);

    WeNetService.createProxy(this.vertx).retrieveJsonArrayAppUserIds(env.app.appId, retrieve -> {

      if (retrieve.failed()) {

        Logger.trace(retrieve.cause(), "No found users from {} to inform of the created task {}", () -> env.app.appId, () -> env.task.id);

      } else {

        final var appUsers = retrieve.result();
        appUsers.remove(env.task.requesterId);
        // TO DO RANKING users before ask them
        final var taskWithUnanswered = new Task();
        taskWithUnanswered.attributes = env.task.attributes;
        if (taskWithUnanswered.attributes == null) {

          taskWithUnanswered.attributes = new JsonObject();
        }
        taskWithUnanswered.attributes.put("unanswered", appUsers);
        WeNetTaskManager.createProxy(this.vertx).mergeTask(env.task.id, taskWithUnanswered, update -> {

          if (update.failed()) {

            Logger.trace(update.cause(), "Cannot update the task {} with the unanswered users", () -> env.task.id);

          } else {

            final var notification = new TaskProposalNotification();
            notification.taskId = env.task.id;
            notification.title = "Can you help?";
            notification.text = env.task.goal.name;
            notification.title = "Can you help?";
            this.sendTo(appUsers, env.app, client, notification);

            final var status = new TaskStatus();
            status.user_id = env.task.requesterId;
            status.task_id = env.task.id;
            status.Action = "taskCreated";
            status.Message = "A task is created";
            this.notifyIncentiveServerTaskStatusChanged(status);
          }

        });
      }
    });

  }

  /**
   * Called when an user has offered as volunteer.
   *
   * @param volunteerId identifier of the user that has offer its help.
   * @param env         to get the data.
   * @param client      to use.
   */
  private void handleVolunteerForTask(final String volunteerId, final EngineEnvironment env, final WebClient client) {

    if (env.task.deadlineTs != null && env.task.deadlineTs > TimeManager.now()) {

      final var taskWhithNewVolunteer = new Task();
      taskWhithNewVolunteer.attributes = env.task.attributes;
      final var unanswered = env.task.attributes.getJsonArray("unanswered");
      if (!unanswered.remove(volunteerId)) {

        final var msg = new TextualMessage();
        msg.recipientId = volunteerId;
        msg.title = "Accept not allowed";
        msg.text = "You cannot be a volunteer, because you already are or you are not a person that can provide help.";
        this.sendTo(env.app, client, msg);

      } else {

        final var volunteers = env.task.attributes.getJsonArray("volunteers");
        volunteers.add(volunteerId);
        WeNetTaskManager.createProxy(this.vertx).mergeTask(env.task.id, taskWhithNewVolunteer, update -> {

          if (update.failed()) {

            Logger.trace(update.cause(), "Cannot update the task {} with the volunteer {}", () -> env.task.id, () -> volunteerId);

          } else {

            Logger.trace("Added volunteer {} into task {}", () -> volunteerId, () -> env.task.id);
            final var socialContextBuilder = WeNetSocialContextBuilder.createProxy(this.vertx);
            socialContextBuilder.updatePreferencesForUserOnTask(volunteerId, env.task.id, volunteers, updated -> {

              if (updated.failed()) {

                Logger.trace(updated.cause(), "Cannot update the preferences for user {} on task {}", () -> volunteerId, () -> env.task.id);

              } else {

                Logger.trace("Updated preferences for user {} on task {}", () -> volunteerId, () -> env.task.id);
              }

            });

            socialContextBuilder.retrieveSocialExplanation(volunteerId, env.task.id, retrieve -> {

              final var notification = new TaskVolunteerNotification();
              notification.recipientId = env.task.requesterId;
              notification.taskId = env.task.id;
              notification.title = "Found volunteer";
              notification.text = "An user want to help you.";
              notification.volunteerId = volunteerId;

              if (retrieve.failed()) {

                Logger.trace(retrieve.cause(), "Cannot obtain the social explanation for user {} on task {}", () -> volunteerId, () -> env.task.id);

              } else {

                final var explanation = retrieve.result();
                Logger.trace("Obtain the explanation {} for user {} on task {}", () -> explanation, () -> volunteerId, () -> env.task.id);
                if (explanation != null && explanation.description != null && explanation.description.length() > 0) {

                  notification.explanation = explanation.description;

                }
              }

              this.sendTo(env.app, client, notification);
              final var status = new TaskStatus();
              status.user_id = volunteerId;
              status.task_id = env.task.id;
              status.Action = "volunteerForTask";
              status.Message = "An user has offered to be a volunteer for a task.";
              this.notifyIncentiveServerTaskStatusChanged(status);

            });

          }
        });

      }

    } else {
      // Error too late
      final var msg = new TextualMessage();
      msg.recipientId = volunteerId;
      msg.title = "Deadline reached";
      msg.text = "It's too late to be a volunteer.";
      this.sendTo(env.app, client, msg);

    }

  }

  /**
   * Called when want to notify to the incentive server that the task status has changed.
   *
   * @param status to notify to the incentive server.
   */
  private void notifyIncentiveServerTaskStatusChanged(final TaskStatus status) {

    WeNetIncentiveServer.createProxy(this.vertx).updateJsonTaskStatus(status.toJsonObject(), update -> {

      if (update.failed()) {

        Logger.trace(update.cause(), "Cannot notify incentive server about  {}.", status);

      } else {

        Logger.trace("Incentive server notified of {} returning", () -> status, () -> update.result());
      }

    });

  }

  /**
   * Called when an user refuse to help.
   *
   * @param volunteerId identifier of the volunteer to refuse to help.
   * @param env         to get the data.
   * @param client      to use.
   */
  private void handleRefuseTask(final String volunteerId, final EngineEnvironment env, final WebClient client) {

    if (env.task.deadlineTs != null && env.task.deadlineTs > TimeManager.now()) {
      final var taskWhereDeclined = new Task();
      taskWhereDeclined.attributes = env.task.attributes;
      final var unanswered = env.task.attributes.getJsonArray("unanswered");
      if (!unanswered.remove(volunteerId)) {

        final var msg = new TextualMessage();
        msg.recipientId = volunteerId;
        msg.title = "Refuse not allowed";
        msg.text = "You cannot refuse to be a volunteer, because you already refused or you are not a person that can provide help.";
        this.sendTo(env.app, client, msg);

      } else {

        final var declined = env.task.attributes.getJsonArray("declined");
        declined.add(volunteerId);
        WeNetTaskManager.createProxy(this.vertx).mergeTask(env.task.id, taskWhereDeclined, update -> {

          if (update.failed()) {

            Logger.trace(update.cause(), "Cannot update the task {} with the declined user {}", () -> env.task.id, () -> volunteerId);

          } else {

            Logger.trace("Added declined users {} into task {}", () -> volunteerId, () -> env.task.id);
            final var status = new TaskStatus();
            status.user_id = volunteerId;
            status.task_id = env.task.id;
            status.Action = "refuseTask";
            status.Message = "An user has declined to be a volunteer for a task.";
            this.notifyIncentiveServerTaskStatusChanged(status);

          }
        });
      }

    } else {
      // Error too late
      final var msg = new TextualMessage();
      msg.recipientId = volunteerId;
      msg.title = "Deadline reached";
      msg.text = "It's too late to refuse to be a volunteer.";
      this.sendTo(env.app, client, msg);
    }

  }

  /**
   * Called when an user is accepted to provide help.
   *
   * @param volunteerId identifier of the volunteer that is accepted to provide help.
   * @param env         to get the data.
   * @param client      to use.
   */
  private void handleAcceptVolunteer(final String volunteerId, final EngineEnvironment env, final WebClient client) {

    final var volunteers = env.task.attributes.getJsonArray("volunteers");
    if (!volunteers.remove(volunteerId)) {

      final var msg = new TextualMessage();
      msg.recipientId = env.task.requesterId;
      msg.title = "Unexpected volunteer to accept";
      msg.text = "The user '" + volunteerId + "' is not a volunteer of the task, so you can not accept it.";
      this.sendTo(env.app, client, msg);

    } else {

      final var accepted = env.task.attributes.getJsonArray("accepted");
      accepted.add(volunteerId);
      final var taskWhereAccepted = new Task();
      taskWhereAccepted.attributes = env.task.attributes;
      WeNetTaskManager.createProxy(this.vertx).mergeTask(env.task.id, taskWhereAccepted, update -> {

        if (update.failed()) {

          Logger.trace(update.cause(), "Cannot update the task {} with the accepted user {}", () -> env.task.id, () -> volunteerId);

        } else {

          Logger.trace("Added accepted users {} into task {}", () -> volunteerId, () -> env.task.id);
          final var notification = new TaskSelectionNotification();
          notification.recipientId = volunteerId;
          notification.taskId = env.task.id;
          notification.title = "Help accepted";
          notification.text = "You has been selected to provide help.";
          notification.outcome = TaskSelectionNotification.Outcome.accepted;
          this.sendTo(env.app, client, notification);

          final var status = new TaskStatus();
          status.user_id = volunteerId;
          status.task_id = env.task.id;
          status.Action = "acceptVolunteer";
          status.Message = "An user is selected to provide help.";
          this.notifyIncentiveServerTaskStatusChanged(status);

        }
      });

    }
  }

  /**
   * Called when an user is refused to provide help.
   *
   * @param volunteerId identifier of the volunteer that is refused to provide help.
   * @param env         to get the data.
   * @param client      to use.
   */
  private void handleRefuseVolunteer(final String volunteerId, final EngineEnvironment env, final WebClient client) {

    final var volunteers = env.task.attributes.getJsonArray("volunteers");
    if (!volunteers.remove(volunteerId)) {

      final var msg = new TextualMessage();
      msg.recipientId = env.task.requesterId;
      msg.title = "Unexpected volunteer to refuse";
      msg.text = "The user '" + volunteerId + "' is not a volunteer of the task, so you can not refuse it.";
      this.sendTo(env.app, client, msg);

    } else {

      final var refused = env.task.attributes.getJsonArray("refused");
      refused.add(volunteerId);
      final var taskWhereRefused = new Task();
      taskWhereRefused.attributes = env.task.attributes;
      WeNetTaskManager.createProxy(this.vertx).mergeTask(env.task.id, taskWhereRefused, update -> {

        if (update.failed()) {

          Logger.trace(update.cause(), "Cannot update the task {} with the refused user {}", () -> env.task.id, () -> volunteerId);

        } else {

          Logger.trace("Added refused users {} into task {}", () -> volunteerId, () -> env.task.id);
          final var notification = new TaskSelectionNotification();
          notification.recipientId = volunteerId;
          notification.taskId = env.task.id;
          notification.title = "Help not needed";
          notification.text = "Your help is not necessary to do the task.";
          notification.outcome = TaskSelectionNotification.Outcome.refused;
          this.sendTo(env.app, client, notification);

          final var status = new TaskStatus();
          status.user_id = volunteerId;
          status.task_id = env.task.id;
          status.Action = "refuseVolunteer";
          status.Message = "An user is refused as volunteer.";
          this.notifyIncentiveServerTaskStatusChanged(status);

        }
      });

    }

  }

  /**
   * Called when an user mark a task as completed.
   *
   * @param outcome state of the completed task.
   * @param env     to get the data.
   * @param client  to use.
   */
  private void handleTaskCompleted(final String outcome, final EngineEnvironment env, final WebClient client) {

    final var closedTask = new Task();
    closedTask.closeTs = TimeManager.now();
    closedTask.attributes = env.task.attributes;
    closedTask.attributes.put("outcome", outcome);
    WeNetTaskManager.createProxy(this.vertx).mergeTask(env.task.id, closedTask, update -> {

      if (update.failed()) {

        Logger.trace("Cannot mark the task {} as closed", () -> env.task.id);

      } else {

        Logger.trace("Closed {} task", () -> env.task.id);

        final var notification = new TaskConcludedNotification();
        notification.taskId = env.task.id;
        notification.title = "Help not needed";
        notification.text = "Your help is not necessary to do the task.";
        notification.outcome = TaskConcludedNotification.Outcome.valueOf(outcome);
        final var unanswered = env.task.attributes.getJsonArray("unanswered");
        this.sendTo(unanswered, env.app, client, notification);
        final var volunteers = env.task.attributes.getJsonArray("volunteers");
        this.sendTo(volunteers, env.app, client, notification);

        notification.title = "Task concluded";
        notification.text = "Your help is not necessary because the task is concluded.";
        final var accepted = env.task.attributes.getJsonArray("accepted");
        this.sendTo(accepted, env.app, client, notification);

        final var status = new TaskStatus();
        status.user_id = env.task.requesterId;
        status.task_id = env.task.id;
        status.Action = "taskCompleted";
        status.Message = "A task is completed with the outcome:" + outcome;
        this.notifyIncentiveServerTaskStatusChanged(status);

      }
    });

  }

  /**
   * Handle an incentive.
   *
   * @param env     environment to use for the message.
   * @param message that contains the task transaction.
   */
  protected void handleIncentive(final EngineEnvironment env, final Message message) {

    final var content = (JsonObject) message.content;
    final var incentive = Model.fromJsonObject(content, Incentive.class);
    final var options = new WebClientOptions();
    final var client = WebClient.create(this.vertx, options);

    final var notification = new TextualMessage();
    notification.title = "    ";
    notification.recipientId = incentive.UserId;
    if (incentive.Message != null) {

      notification.text = incentive.Message.content;

    } else {

      notification.text = incentive.Badge.Message;

    }
    this.sendTo(env.app, client, notification);

  }

  /**
   * Handle a SWIProlog event.
   *
   * @param env     environment to use for the message.
   * @param message that contains the task transaction.
   */
  protected void handleSWIProlog(final EngineEnvironment env, final Message message) {

    File tmp = null;
    try {

      tmp = Files.createTempDir();
      final var error = new File(tmp, "error.txt");
      final var output = new File(tmp, "output.txt");
      final var actionsFile = new File(tmp, "actions.json");
      final var messageFile = new File(tmp, "message.json");
      Files.write(message.toJsonString().getBytes(), messageFile);
      final var environmentFile = new File(tmp, "environment.json");
      Files.write(env.toJsonString().getBytes(), environmentFile);
      final var configurationFile = new File(tmp, "configuration.json");
      Files.write(this.config().encode().getBytes(), configurationFile);
      final var initPl = new File(tmp, "init.pl");
      final var individualNorms = new File(tmp, "individual_norms.pl");
      individualNorms.createNewFile();
      final var friends = new File(tmp, "friends.pl");
      this.fillInFriendsInto(friends,env);
      final var init = new StringBuilder();
      init.append("actions_file('");
      init.append(actionsFile.getAbsolutePath());
      init.append("').\n");
      init.append("message_file('");
      init.append(messageFile.getAbsolutePath());
      init.append("').\n");
      init.append("environment_file('");
      init.append(environmentFile.getAbsolutePath());
      init.append("').\n");
      init.append("configuration_file('");
      init.append(configurationFile.getAbsolutePath());
      init.append("').\n");
      init.append("norm_interpreter_file('");
      init.append(this.normInterpreter.getAbsolutePath());
      init.append("').\n");
      init.append("individual_norms_file('");
      init.append(individualNorms.getAbsolutePath());
      init.append("').\n");
      init.append("community_norms_file('");
      init.append(this.communityNorms.getAbsolutePath());
      init.append("').\n");
      init.append("friends_file('");
      init.append(friends.getAbsolutePath());
      init.append("').\n");
      Files.asCharSink(initPl, Charset.defaultCharset()).write(init);

      final var processBuilder = new ProcessBuilder("swipl","--debug", "-O", "-f", initPl.getAbsolutePath(), "-s", this.engine.getAbsolutePath(), "-g", "go", "-t", "halt");
      processBuilder.directory(tmp);
      processBuilder.redirectError(error);
      processBuilder.redirectOutput(output);
      final var process = processBuilder.start();
      Logger.trace("Start process with {}", () -> processBuilder.command());
      final var status = process.waitFor();
      Logger.trace("Finished process {} with code {}.\nThe error stream is:\n{}\nThe output stream is:\n{}", () -> processBuilder.command(), () -> status, () -> readQuietly(error), () -> readQuietly(output));
      if (status == 0) {

        final var actions = new JsonArray(Buffer.buffer(IOUtils.toString(new FileReader(actionsFile))));
        Logger.trace("Actions {}", () -> actions.encodePrettily());
      }

    } catch (final Throwable processError) {

      Logger.error(processError, "Cannot process the SWI Prolog message");
    }

    if (!FileUtils.deleteQuietly(tmp)) {

      Logger.error("Cannot remove the working directory {}", tmp);

    }

  }

  /**
   * Add the information of the friends to the specified file.
   *
   * @param friends file to write.
   * @param env environment with the data.
   *
   * @throws IOException If cannot write into the file.
   */
  private void fillInFriendsInto(final File friends, final EngineEnvironment env) throws IOException {

    final var data = new StringBuilder();
    if( env.sender != null && env.sender.relationships != null){

      for( final var relationship:env.sender.relationships ) {

        data.append("friends(");
        data.append(relationship.userId);
        data.append(",[");
        // TO DO add community
        data.append("]).\n");

      }
    }
    Files.asCharSink(friends, Charset.defaultCharset()).write(data);
  }

  /**
   * Read a file quietly.
   *
   * @param file to read.
   *
   * @return the content of the file.
   */
  public static String readQuietly(final File file) {

    try {

      return Files.asCharSource(file, Charset.defaultCharset()).read();

    } catch (final Throwable t) {

      return t.toString();
    }

  }

}
