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
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
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

import org.tinylog.Logger;

import eu.internetofus.common.TimeManager;
import eu.internetofus.common.components.Model;
import eu.internetofus.common.components.incentive_server.TaskStatus;
import eu.internetofus.common.components.incentive_server.WeNetIncentiveServer;
import eu.internetofus.common.components.interaction_protocol_engine.InteractionProtocolMessage;
import eu.internetofus.common.components.service.App;
import eu.internetofus.common.components.service.TaskConcludedNotification;
import eu.internetofus.common.components.service.TaskProposalNotification;
import eu.internetofus.common.components.service.TaskSelectionNotification;
import eu.internetofus.common.components.service.TaskVolunteerNotification;
import eu.internetofus.common.components.service.TextualMessage;
import eu.internetofus.common.components.service.WeNetService;
import eu.internetofus.common.components.social_context_builder.WeNetSocialContextBuilder;
import eu.internetofus.common.components.task_manager.Task;
import eu.internetofus.common.components.task_manager.WeNetTaskManager;
import eu.internetofus.common.vertx.Worker;
import io.vertx.core.AbstractVerticle;
import io.vertx.core.Handler;
import io.vertx.core.Promise;
import io.vertx.core.eventbus.Message;
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
public class EngineWorker extends AbstractVerticle implements Handler<Message<JsonObject>> {

  /**
   * The address used to send messages to the worker.
   */
  public static final String ADDRESSS = "eu.internetofus.wenet_interaction_protocol_engine.worker";

  /**
   * The component that will consume the messages.
   */
  protected MessageConsumer<JsonObject> consumer;

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

        startPromise.complete();
      }

    });

  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void handle(final Message<JsonObject> event) {

    try {

      final JsonObject body = event.body();
      final InteractionProtocolMessage message = Model.fromJsonObject(body, InteractionProtocolMessage.class);
      if (message == null) {

        Logger.trace("Can not process the event {}, because does not contains a valid InteractionProtocolMessage.", event);

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
  protected void handle(final InteractionProtocolMessage message) {

    Logger.trace("Received message to process {}", message);
    EngineEnvironment.create(this.vertx, message).onComplete(creation -> {

      try {

        final EngineEnvironment env = creation.result();
        if (message.content instanceof JsonObject) {

          final JsonObject content = (JsonObject) message.content;
          final String action = content.getString("action");
          final WebClientOptions options = new WebClientOptions();
          final WebClient client = WebClient.create(this.vertx, options);
          this.fixTaskAttributes(env, message);
          if ("TaskCreation".equalsIgnoreCase(action)) {

            this.handleTaskCreated(env, client);

          } else if ("volunteerForTask".equalsIgnoreCase(action)) {

            final String volunteerId = content.getJsonObject("attributes", new JsonObject()).getString("volunteerId", "0");
            this.handleVolunteerForTask(volunteerId, env, client);

          } else if ("refuseTask".equalsIgnoreCase(action)) {
            // nothing to do
            final String volunteerId = content.getJsonObject("attributes", new JsonObject()).getString("volunteerId", "0");
            this.handleRefuseTask(volunteerId, env, client);

          } else if ("acceptVolunteer".equalsIgnoreCase(action)) {

            final String volunteerId = content.getJsonObject("attributes", new JsonObject()).getString("volunteerId", "0");
            this.handleAcceptVolunteer(volunteerId, env, client);

          } else if ("refuseVolunteer".equalsIgnoreCase(action)) {

            final String volunteerId = content.getJsonObject("attributes", new JsonObject()).getString("volunteerId", "0");
            this.handleRefuseVolunteer(volunteerId, env, client);

          } else if ("taskCompleted".equalsIgnoreCase(action)) {

            final String outcome = content.getJsonObject("attributes", new JsonObject()).getString("outcome", "cancelled");
            this.handleTaskCompleted(outcome, env, client);

          } else {

            Logger.trace("Unexpected action {}", message);
          }

        } else {

          Logger.trace("Unexpected {}", message);
        }

      } catch (final Throwable throwable) {

        Logger.trace(throwable, "Can not process {}", message);
      }

    });

  }

  /**
   * Called when want to send some messages into an application.
   *
   * @param app          application to notify.
   * @param client       to use.
   * @param notification to send to the application.
   */
  private void sendTo(final App app, final WebClient client, final eu.internetofus.common.components.service.Message notification) {

    final JsonObject body = notification.toJsonObject();
    client.postAbs(app.messageCallbackUrl).sendJsonObject(body, send -> {

      if (send.failed()) {

        Logger.trace(send.cause(), "Can not notify about  {} to {}.", body, app);

      } else {

        Logger.trace("Sent {}  to {}", body, app);
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
  private void sendTo(final JsonArray users, final App app, final WebClient client, final eu.internetofus.common.components.service.Message notification) {

    for (int i = 0; i < users.size(); i++) {

      notification.recipientId = users.getString(i);
      this.sendTo(app, client, notification);
    }

  }

  /**
   * Fix task attributes.
   *
   * @param env
   *
   * @param task    to fix the task attributes.
   * @param message received about the task.
   *
   */
  private void fixTaskAttributes(final EngineEnvironment env, final InteractionProtocolMessage message) {

    if (env.task == null) {

      env.task = new Task();
      env.task.id = message.taskId;

    }

    if (env.task.attributes == null) {

      env.task.attributes = new JsonObject();
    }

    final JsonArray unanswered = env.task.attributes.getJsonArray("unanswered", null);
    if (unanswered == null) {

      env.task.attributes.put("unanswered", new JsonArray());
    }
    final JsonArray volunteers = env.task.attributes.getJsonArray("volunteers", null);
    if (volunteers == null) {

      env.task.attributes.put("volunteers", new JsonArray());
    }
    final JsonArray declined = env.task.attributes.getJsonArray("declined", null);
    if (declined == null) {

      env.task.attributes.put("declined", new JsonArray());
    }
    final JsonArray accepted = env.task.attributes.getJsonArray("accepted", null);
    if (accepted == null) {

      env.task.attributes.put("accepted", new JsonArray());
    }
    final JsonArray refused = env.task.attributes.getJsonArray("refused", null);
    if (refused == null) {

      env.task.attributes.put("refused", new JsonArray());
    }

  }

  /**
   * Handle the message that inform that a task has been created.
   *
   * @param env    to get the data.
   * @param client to use.
   */
  private void handleTaskCreated(final EngineEnvironment env, final WebClient client) {

    WeNetService.createProxy(this.vertx).retrieveJsonArrayAppUserIds(env.app.appId, retrieve -> {

      if (retrieve.failed()) {

        Logger.trace(retrieve.cause(), "No found users from {} to inform of the created task {}", () -> env.app.appId, () -> env.task.id);

      } else {

        final JsonArray appUsers = retrieve.result();
        appUsers.remove(env.task.requesterId);
        // TO DO RANKING users before ask them
        final Task taskWithUnanswered = new Task();
        taskWithUnanswered.attributes = env.task.attributes;
        taskWithUnanswered.attributes.put("unanswered", appUsers);
        WeNetTaskManager.createProxy(this.vertx).updateTask(env.task.id, taskWithUnanswered, update -> {

          if (update.failed()) {

            Logger.trace(update.cause(), "Cannot update the task {} with the unanswered users", () -> env.task.id);

          } else {

            final TaskProposalNotification notification = new TaskProposalNotification();
            notification.taskId = env.task.id;
            notification.title = "Can you help?";
            notification.text = env.task.goal.name;
            notification.title = "Can you help?";
            this.sendTo(appUsers, env.app, client, notification);

            final TaskStatus status = new TaskStatus();
            status.user_id = env.task.requesterId;
            status.task_id = env.task.id;
            status.Action = "taskCreated";
            status.Message_content = "A task is created";
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

    if (env.task.closeTs != null && env.task.deadlineTs != null && env.task.deadlineTs > TimeManager.now()) {

      final Task taskWhithNewVolunteer = new Task();
      taskWhithNewVolunteer.attributes = env.task.attributes;
      final JsonArray unanswered = env.task.attributes.getJsonArray("unanswered");
      if (!unanswered.remove(volunteerId)) {

        final TextualMessage msg = new TextualMessage();
        msg.recipientId = volunteerId;
        msg.title = "Accept not allowed";
        msg.text = "You cannot be a volunteer, because you already are or you are not a person that can provide help.";
        this.sendTo(env.app, client, msg);

      } else {

        final JsonArray volunteers = env.task.attributes.getJsonArray("volunteers");
        volunteers.add(volunteerId);
        WeNetTaskManager.createProxy(this.vertx).updateTask(env.task.id, taskWhithNewVolunteer, update -> {

          if (update.failed()) {

            Logger.trace(update.cause(), "Cannot update the task {} with the volunteer {}", () -> env.task.id, () -> volunteerId);

          } else {

            Logger.trace("Added volunteer {} into task {}", () -> volunteerId, () -> env.task.id);
            final TaskVolunteerNotification notification = new TaskVolunteerNotification();
            notification.recipientId = env.task.requesterId;
            notification.taskId = env.task.id;
            notification.title = "Found volunteer";
            notification.text = "An user want to help you.";
            notification.volunteerId = volunteerId;
            this.sendTo(env.app, client, notification);
            WeNetSocialContextBuilder.createProxy(this.vertx).updatePreferencesForUserOnTask(volunteerId, env.task.id, volunteers, updated -> {

              if (updated.failed()) {

                Logger.trace(updated.cause(), "Cannot update the preferences for user {} on task {}", () -> volunteerId, () -> env.task.id);

              } else {

                Logger.trace("Updated preferences for user {} on task {}", () -> volunteerId, () -> env.task.id);
              }

            });

            final TaskStatus status = new TaskStatus();
            status.user_id = volunteerId;
            status.task_id = env.task.id;
            status.Action = "volunteerForTask";
            status.Message_content = "An user has offered to be a volunteer for a task.";
            this.notifyIncentiveServerTaskStatusChanged(status);

          }
        });

      }

    } else {
      // too late
      final TextualMessage msg = new TextualMessage();
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

    WeNetIncentiveServer.createProxy(this.vertx).updateTaskStatus(status, update -> {

      if (update.failed()) {

        Logger.trace(update.cause(), "Cannot notify incentive server about  {}.", status);

      } else {

        Logger.trace("Incentive server notified of {}", status);
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

    if (env.task.closeTs != null && env.task.deadlineTs != null && env.task.deadlineTs > TimeManager.now()) {
      final Task taskWhereDeclined = new Task();
      taskWhereDeclined.attributes = env.task.attributes;
      final JsonArray unanswered = env.task.attributes.getJsonArray("unanswered");
      if (!unanswered.remove(volunteerId)) {

        final TextualMessage msg = new TextualMessage();
        msg.recipientId = volunteerId;
        msg.title = "Refuse not allowed";
        msg.text = "You cannot refuse to be a volunteer, because you already refused or you are not a person that can provide help.";
        this.sendTo(env.app, client, msg);

      } else {

        final JsonArray declined = env.task.attributes.getJsonArray("declined");
        declined.add(volunteerId);
        WeNetTaskManager.createProxy(this.vertx).updateTask(env.task.id, taskWhereDeclined, update -> {

          if (update.failed()) {

            Logger.trace(update.cause(), "Cannot update the task {} with the declined user {}", () -> env.task.id, () -> volunteerId);

          } else {

            Logger.trace("Added declined users {} into task {}", () -> volunteerId, () -> env.task.id);
            final TaskStatus status = new TaskStatus();
            status.user_id = volunteerId;
            status.task_id = env.task.id;
            status.Action = "refuseTask";
            status.Message_content = "An user has declined to be a volunteer for a task.";
            this.notifyIncentiveServerTaskStatusChanged(status);

          }
        });
      }

    } else {
      // error used already refused
      final TextualMessage msg = new TextualMessage();
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

    if (env.task.closeTs != null && env.task.deadlineTs != null && env.task.deadlineTs > TimeManager.now()) {

      final JsonArray volunteers = env.task.attributes.getJsonArray("volunteers");
      if (!volunteers.remove(volunteerId)) {

        final TextualMessage msg = new TextualMessage();
        msg.recipientId = env.task.requesterId;
        msg.title = "Unexpected volunteer to accept";
        msg.text = "The user '" + volunteerId + "' is not a volunteer of the task, so you can not accept it.";
        this.sendTo(env.app, client, msg);

      } else {

        final JsonArray accepted = env.task.attributes.getJsonArray("accepted");
        accepted.add(volunteerId);
        final Task taskWhereAccepted = new Task();
        taskWhereAccepted.attributes = env.task.attributes;
        WeNetTaskManager.createProxy(this.vertx).updateTask(env.task.id, taskWhereAccepted, update -> {

          if (update.failed()) {

            Logger.trace(update.cause(), "Cannot update the task {} with the accepted user {}", () -> env.task.id, () -> volunteerId);

          } else {

            Logger.trace("Added accepted users {} into task {}", () -> volunteerId, () -> env.task.id);
            final TaskSelectionNotification notification = new TaskSelectionNotification();
            notification.recipientId = volunteerId;
            notification.taskId = env.task.id;
            notification.title = "Help accepted";
            notification.text = "You has been selected to provide help.";
            notification.outcome = TaskSelectionNotification.Outcome.accepted;
            this.sendTo(env.app, client, notification);

            final TaskStatus status = new TaskStatus();
            status.user_id = volunteerId;
            status.task_id = env.task.id;
            status.Action = "acceptVolunteer";
            status.Message_content = "An user is selected to provide help.";
            this.notifyIncentiveServerTaskStatusChanged(status);

          }
        });

      }

    } else {
      // too late
      final TextualMessage msg = new TextualMessage();
      msg.recipientId = volunteerId;
      msg.title = "Deadline reached";
      msg.text = "It's too late to accept a volunteer.";
      this.sendTo(env.app, client, msg);
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

    if (env.task.closeTs != null && env.task.deadlineTs != null && env.task.deadlineTs > TimeManager.now()) {

      final JsonArray volunteers = env.task.attributes.getJsonArray("volunteers");
      if (!volunteers.remove(volunteerId)) {

        final TextualMessage msg = new TextualMessage();
        msg.recipientId = env.task.requesterId;
        msg.title = "Unexpected volunteer to refuse";
        msg.text = "The user '" + volunteerId + "' is not a volunteer of the task, so you can not refuse it.";
        this.sendTo(env.app, client, msg);

      } else {

        final JsonArray refused = env.task.attributes.getJsonArray("refused");
        refused.add(volunteerId);
        final Task taskWhereRefused = new Task();
        taskWhereRefused.attributes = env.task.attributes;
        WeNetTaskManager.createProxy(this.vertx).updateTask(env.task.id, taskWhereRefused, update -> {

          if (update.failed()) {

            Logger.trace(update.cause(), "Cannot update the task {} with the refused user {}", () -> env.task.id, () -> volunteerId);

          } else {

            Logger.trace("Added refused users {} into task {}", () -> volunteerId, () -> env.task.id);
            final TaskSelectionNotification notification = new TaskSelectionNotification();
            notification.recipientId = volunteerId;
            notification.taskId = env.task.id;
            notification.title = "Help not needed";
            notification.text = "Your help is not necessary to do the task.";
            notification.outcome = TaskSelectionNotification.Outcome.refused;
            this.sendTo(env.app, client, notification);

            final TaskStatus status = new TaskStatus();
            status.user_id = volunteerId;
            status.task_id = env.task.id;
            status.Action = "refuseVolunteer";
            status.Message_content = "An user is refused as volunteer.";
            this.notifyIncentiveServerTaskStatusChanged(status);

          }
        });

      }

    } else {
      // too late
      final TextualMessage msg = new TextualMessage();
      msg.recipientId = volunteerId;
      msg.title = "Deadline reached";
      msg.text = "It's too late to refuse a volunteer.";
      this.sendTo(env.app, client, msg);
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

    if (env.task.closeTs != null) {

      final Task closedTask = new Task();
      closedTask.closeTs = TimeManager.now();
      closedTask.attributes = env.task.attributes;
      closedTask.attributes.put("outcome", outcome);
      WeNetTaskManager.createProxy(this.vertx).updateTask(env.task.id, closedTask, update -> {

        if (update.failed()) {

          Logger.trace("Cannot mark the task {} as closed", () -> env.task.id);

        } else {

          Logger.trace("Closed {} task", () -> env.task.id);

          final TaskConcludedNotification notification = new TaskConcludedNotification();
          notification.taskId = env.task.id;
          notification.title = "Help not needed";
          notification.text = "Your help is not necessary to do the task.";
          notification.outcome = TaskConcludedNotification.Outcome.valueOf(outcome);
          final JsonArray unanswered = env.task.attributes.getJsonArray("unanswered");
          this.sendTo(unanswered, env.app, client, notification);
          final JsonArray volunteers = env.task.attributes.getJsonArray("volunteers");
          this.sendTo(volunteers, env.app, client, notification);

          notification.title = "Task concluded";
          notification.text = "Your help is not necessary because teh task is concluded.";
          final JsonArray accepted = env.task.attributes.getJsonArray("accepted");
          this.sendTo(accepted, env.app, client, notification);

          final TaskStatus status = new TaskStatus();
          status.user_id = env.task.requesterId;
          status.task_id = env.task.id;
          status.Action = "taskCompleted";
          status.Message_content = "A task is completed with the outcome:" + outcome;
          this.notifyIncentiveServerTaskStatusChanged(status);

        }
      });

    } else {
      // too late
      final TextualMessage msg = new TextualMessage();
      msg.recipientId = env.task.requesterId;
      msg.title = "Task already closed";
      msg.text = "It's too late the task is already completed.";
      this.sendTo(env.app, client, msg);

    }
  }

}
