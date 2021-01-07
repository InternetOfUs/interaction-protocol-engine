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

import eu.internetofus.common.TimeManager;
import eu.internetofus.common.components.Model;
import eu.internetofus.common.components.incentive_server.Incentive;
import eu.internetofus.common.components.incentive_server.TaskStatus;
import eu.internetofus.common.components.incentive_server.WeNetIncentiveServer;
import eu.internetofus.common.components.service.Message;
import eu.internetofus.common.components.service.WeNetService;
import eu.internetofus.common.components.social_context_builder.WeNetSocialContextBuilder;
import eu.internetofus.common.components.task_manager.Task;
import eu.internetofus.common.components.task_manager.TaskTransaction;
import eu.internetofus.common.components.task_manager.WeNetTaskManager;
import eu.internetofus.common.vertx.Worker;
import io.vertx.core.AbstractVerticle;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.Promise;
import io.vertx.core.Vertx;
import io.vertx.core.eventbus.MessageConsumer;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.client.WebClient;
import io.vertx.ext.web.client.WebClientOptions;
import org.tinylog.Logger;

/**
 * This is the worker used to manage the actions following the hard-coded
 * protocol.
 *
 * @author UDT-IA, IIIA-CSIC
 */
@Worker
public class HardCodedProtocolWorker extends AbstractVerticle
    implements Handler<io.vertx.core.eventbus.Message<JsonObject>> {

  /**
   * The address used to send messages to the worker.
   */
  public static final String ADDRESSS = "eu.internetofus.wenet_interaction_protocol_engine.hard_coded_protocol_worker";

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
    startPromise.complete();

  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void handle(final io.vertx.core.eventbus.Message<JsonObject> event) {

    try {

      final var body = event.body();
      final var type = MessageForWorkerBuilder.Type
          .valueOf(body.getString("type", MessageForWorkerBuilder.Type.DO_TASK_TRANSACTION.name()));
      switch (type) {
      case CREATED_TASK:

        final var task = Model.fromJsonObject(body.getJsonObject("task"), Task.class);
        if (task == null) {

          Logger.trace("Cannot process the event {}, because does not contains a valid transaction.", event);

        } else {

          this.createEnvironmentFor(task).onComplete(creation -> {

            if (creation.failed()) {

              Logger.trace(creation.cause(), "Cannot process the creation of the task {}", task);

            } else {

              final var env = creation.result();
              this.handleTaskCreated(env);
            }
          });

        }
        break;
      case DO_TASK_TRANSACTION:

        final var transaction = Model.fromJsonObject(body.getJsonObject("transaction"), TaskTransaction.class);
        if (transaction == null) {

          Logger.trace("Cannot process the event {}, because does not contains a valid transaction.", event);

        } else {

          this.createEnvironmentFor(transaction.taskId).onComplete(creation -> {

            if (creation.failed()) {

              Logger.trace(creation.cause(), "Cannot process the task transaction {}", transaction);

            } else {

              final var env = creation.result();
              this.handleTaskTransaction(env, transaction);
            }
          });
        }
        break;
      default:
        // Is an incentive
        final var incentive = Model.fromJsonObject(body.getJsonObject("incentive"), Incentive.class);
        if (incentive == null) {

          Logger.trace("Cannot process the event {}, because does not contains a valid incentive.", event);

        } else {

          this.handleIncentive(incentive);
        }

      }

    } catch (final Throwable throwable) {

      Logger.trace(throwable, "Can not process the event {}", event);
    }

  }

  /**
   * Create the environment for the specified task.
   *
   * @param task to create the environment.
   *
   * @return the future environment for the task.
   */
  protected Future<HardCodedProtocolEnvironment> createEnvironmentFor(final Task task) {

    final Promise<HardCodedProtocolEnvironment> promise = Promise.promise();
    this.createEnvironmentFor(task, promise);
    return promise.future();

  }

  /**
   * Create the environment for the specified task.
   *
   * @param task    to create the environment.
   * @param promise to inform of the environment for the task.
   */
  protected void createEnvironmentFor(final Task task, final Promise<HardCodedProtocolEnvironment> promise) {

    WeNetService.createProxy(this.vertx).retrieveApp(task.appId).onComplete(retrieve -> {

      final var app = retrieve.result();
      if (app != null) {

        final var env = new HardCodedProtocolEnvironment(task, this.vertx, app.messageCallbackUrl);
        promise.complete(env);

      } else {

        Logger.trace("Cannot obtain the callback URL for the app {}", task.appId);
        promise.fail(retrieve.cause());
      }

    });

  }

  /**
   * Create the environment for the specified task identifier.
   *
   * @param taskId identifier of the task for the environment.
   *
   * @return the future environment for the task.
   */
  protected Future<HardCodedProtocolEnvironment> createEnvironmentFor(final String taskId) {

    final Promise<HardCodedProtocolEnvironment> promise = Promise.promise();
    WeNetTaskManager.createProxy(this.vertx).retrieveTask(taskId).onComplete(retrieve -> {

      final var task = retrieve.result();
      if (task != null) {

        this.createEnvironmentFor(task, promise);

      } else {

        Logger.trace("Cannot obtain the task {}", taskId);
        promise.fail(retrieve.cause());
      }

    });

    return promise.future();

  }

  /**
   * Contains the data necessary for realize the hard-coded protocol.
   */
  protected final class HardCodedProtocolEnvironment {

    /**
     * The task associated to the protocol.
     */
    public Task task;

    /**
     * The url to send callbacks.
     */
    public String callbackUrl;

    /**
     * Client used to do the call backs.
     */
    public WebClient client;

    /**
     * Create a new environment.
     *
     * @param task        for the environment.
     * @param vertx       the event bus to use.
     * @param callbackUrl the URL to post the callback messages.
     */
    public HardCodedProtocolEnvironment(final Task task, final Vertx vertx, final String callbackUrl) {

      this.task = task;
      if (task.attributes == null) {

        task.attributes = new JsonObject();
      }

      final var unanswered = task.attributes.getJsonArray("unanswered", null);
      if (unanswered == null) {

        task.attributes.put("unanswered", new JsonArray());
      }
      final var volunteers = task.attributes.getJsonArray("volunteers", null);
      if (volunteers == null) {

        task.attributes.put("volunteers", new JsonArray());
      }
      final var declined = task.attributes.getJsonArray("declined", null);
      if (declined == null) {

        task.attributes.put("declined", new JsonArray());
      }
      final var accepted = task.attributes.getJsonArray("accepted", null);
      if (accepted == null) {

        task.attributes.put("accepted", new JsonArray());
      }
      final var refused = task.attributes.getJsonArray("refused", null);
      if (refused == null) {

        task.attributes.put("refused", new JsonArray());
      }

      final var options = new WebClientOptions();
      this.client = WebClient.create(vertx, options);
      this.callbackUrl = callbackUrl;

    }

    /**
     * Called when want to send a notification to multiple users of an application.
     *
     * @param users        to send the message.
     * @param notification to send to the application.
     */
    public void sendTo(final JsonArray users, Message notification) {

      for (var i = 0; i < users.size(); i++) {

        notification.receiverId = users.getString(i);
        this.sendTo(notification);
      }

    }

    /**
     * Called when want to send some messages into an application.
     *
     * @param notification to send to the application.
     */
    public void sendTo(Message notification) {

      if (this.callbackUrl == null) {

        Logger.trace("Cannot send {}, because not have callback URL for ", () -> notification, () -> this.task.appId);

      } else {

        final var body = notification.toJsonObject();
        this.client.postAbs(this.callbackUrl).sendJsonObject(body, send -> {

          if (send.failed()) {

            Logger.trace(send.cause(), "App[{}]: POST {} with {} failed", () -> this.task.appId, () -> this.callbackUrl,
                () -> body);

          } else {

            final var response = send.result();
            Logger.trace("App[{}]: POST {} with {} responds with code {} and body {}", () -> this.task.appId,
                () -> this.callbackUrl, () -> body, () -> response.statusCode(), () -> response.bodyAsString());
          }

        });
      }
    }

  }

  /**
   * Create a new message.
   *
   * @param env where the message is said.
   *
   * @return the created message.
   */
  protected Message createMessage(HardCodedProtocolEnvironment env) {

    var msg = new Message();
    msg.appId = env.task.appId;
    msg.attributes = new JsonObject();
    return msg;

  }

  /**
   * Create a new message with the community and task identifiers.
   *
   * @param env where the message is said.
   *
   * @return the created message.
   */
  protected Message createMessageWithCommunityandTaskIds(HardCodedProtocolEnvironment env) {

    var msg = new Message();
    msg.appId = env.task.appId;
    msg.attributes = new JsonObject().put("communityId", env.task.communityId).put("taskId", env.task.id);
    return msg;

  }

  /**
   * Create a new message.
   *
   * @param env   where the message is said.
   * @param title of the message.
   * @param text  of the message.
   *
   * @return the created message.
   */
  protected Message createTextualMessage(HardCodedProtocolEnvironment env, String title, String text) {

    var msg = this.createMessage(env);
    msg.label = "TextualMessage";
    msg.attributes = new JsonObject().put("title", title).put("text", text);
    return msg;

  }

  /**
   * Handle a task transaction.
   *
   * @param env         protocol environment.
   * @param transaction to handle.
   */
  protected void handleTaskTransaction(final HardCodedProtocolEnvironment env, final TaskTransaction transaction) {

    if (env.task.closeTs == null) {

      final JsonObject attr = transaction.attributes;

      if ("volunteerForTask".equalsIgnoreCase(transaction.label)) {

        final var volunteerId = attr.getString("volunteerId", "0");
        this.handleVolunteerForTask(volunteerId, env);

      } else if ("refuseTask".equalsIgnoreCase(transaction.label)) {

        final var volunteerId = attr.getString("volunteerId", "0");
        this.handleRefuseTask(volunteerId, env);

      } else if ("acceptVolunteer".equalsIgnoreCase(transaction.label)) {

        final var volunteerId = attr.getString("volunteerId", "0");
        this.handleAcceptVolunteer(volunteerId, env);

      } else if ("refuseVolunteer".equalsIgnoreCase(transaction.label)) {

        final var volunteerId = attr.getString("volunteerId", "0");
        this.handleRefuseVolunteer(volunteerId, env);

      } else if ("taskCompleted".equalsIgnoreCase(transaction.label)) {

        final var outcome = attr.getString("outcome", "cancelled");
        this.handleTaskCompleted(outcome, env);

      } else {

        Logger.trace("Unexpected transaction {}", transaction);
      }

    } else {
      // Error task closed
      final var msg = this.createTextualMessage(env, "Task already closed",
          "It's too late the task is already completed.");
      msg.receiverId = transaction.attributes.getString("volunteerId", env.task.requesterId);
      env.sendTo(msg);
    }

  }

  /**
   * Handle the message that inform that a task has been created.
   *
   * @param env protocol environment.
   */
  private void handleTaskCreated(final HardCodedProtocolEnvironment env) {

    WeNetService.createProxy(this.vertx).retrieveAppUserIds(env.task.appId, retrieve -> {

      if (retrieve.failed()) {

        Logger.trace(retrieve.cause(), "No found users from {} to inform of the created task {}", () -> env.task.appId,
            () -> env.task.id);

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
        WeNetTaskManager.createProxy(this.vertx).mergeTask(env.task.id, taskWithUnanswered).onComplete(update -> {

          if (update.failed()) {

            Logger.trace(update.cause(), "Cannot update the task {} with the unanswered users", () -> env.task.id);

          } else {

            final var notification = this.createMessageWithCommunityandTaskIds(env);
            notification.label = "TaskProposalNotification";
            env.sendTo(appUsers, notification);

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
   */
  private void handleVolunteerForTask(final String volunteerId, final HardCodedProtocolEnvironment env) {

    var deadlineTs = env.task.attributes.getLong("deadlineTs", null);
    if (deadlineTs != null && deadlineTs > TimeManager.now()) {

      final var taskWhithNewVolunteer = new Task();
      taskWhithNewVolunteer.attributes = env.task.attributes;
      final var unanswered = env.task.attributes.getJsonArray("unanswered");
      if (!unanswered.remove(volunteerId)) {

        final var msg = this.createTextualMessage(env, "Accept not allowed",
            "You cannot be a volunteer, because you already are or you are not a person that can provide help.");
        msg.receiverId = volunteerId;
        env.sendTo(msg);

      } else {

        final var volunteers = env.task.attributes.getJsonArray("volunteers");
        volunteers.add(volunteerId);
        WeNetTaskManager.createProxy(this.vertx).mergeTask(env.task.id, taskWhithNewVolunteer).onComplete(update -> {

          if (update.failed()) {

            Logger.trace(update.cause(), "Cannot update the task {} with the volunteer {}", () -> env.task.id,
                () -> volunteerId);

          } else {

            Logger.trace("Added volunteer {} into task {}", () -> volunteerId, () -> env.task.id);
            final var socialContextBuilder = WeNetSocialContextBuilder.createProxy(this.vertx);
            socialContextBuilder.updatePreferencesForUserOnTask(volunteerId, env.task.id, volunteers, updated -> {

              if (updated.failed()) {

                Logger.trace(updated.cause(), "Cannot update the preferences for user {} on task {}", () -> volunteerId,
                    () -> env.task.id);

              } else {

                Logger.trace("Updated preferences for user {} on task {}", () -> volunteerId, () -> env.task.id);
              }

            });

            final var status = new TaskStatus();
            status.user_id = volunteerId;
            status.task_id = env.task.id;
            status.Action = "volunteerForTask";
            status.Message = "An user has offered to be a volunteer for a task.";
            this.notifyIncentiveServerTaskStatusChanged(status);

            socialContextBuilder.retrieveSocialExplanation(volunteerId, env.task.id).onComplete(retrieve -> {

              final var notification = this.createMessageWithCommunityandTaskIds(env);
              notification.label = "TaskVolunteerNotification";
              notification.receiverId = env.task.requesterId;
              notification.attributes = notification.attributes.put("volunteerId", volunteerId);

              if (retrieve.failed()) {

                Logger.trace(retrieve.cause(), "Cannot obtain the social explanation for user {} on task {}",
                    () -> volunteerId, () -> env.task.id);

              } else {

                final var explanation = retrieve.result();
                Logger.trace("Obtain the explanation {} for user {} on task {}", () -> explanation, () -> volunteerId,
                    () -> env.task.id);
                if (explanation != null && explanation.description != null && explanation.description.length() > 0) {

                  notification.attributes = notification.attributes.put("explanation", explanation.description);

                }
              }

              env.sendTo(notification);

            });

          }
        });

      }

    } else {
      // Error too late
      final var msg = this.createTextualMessage(env, "Deadline reached", "It's too late to be a volunteer.");
      msg.receiverId = volunteerId;
      env.sendTo(msg);

    }

  }

  /**
   * Called when want to notify to the incentive server that the task status has
   * changed.
   *
   * @param status to notify to the incentive server.
   */
  private void notifyIncentiveServerTaskStatusChanged(final TaskStatus status) {

    WeNetIncentiveServer.createProxy(this.vertx).updateTaskStatus(status.toJsonObject(), update -> {

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
   */
  private void handleRefuseTask(final String volunteerId, final HardCodedProtocolEnvironment env) {

    var deadlineTs = env.task.attributes.getLong("deadlineTs", null);
    if (deadlineTs != null && deadlineTs > TimeManager.now()) {

      final var taskWhereDeclined = new Task();
      taskWhereDeclined.attributes = env.task.attributes;
      final var unanswered = env.task.attributes.getJsonArray("unanswered");
      if (!unanswered.remove(volunteerId)) {

        final var msg = this.createTextualMessage(env, "Refuse not allowed",
            "You cannot refuse to be a volunteer, because you already refused or you are not a person that can provide help.");
        msg.receiverId = volunteerId;
        env.sendTo(msg);

      } else {

        final var declined = env.task.attributes.getJsonArray("declined");
        declined.add(volunteerId);
        WeNetTaskManager.createProxy(this.vertx).mergeTask(env.task.id, taskWhereDeclined).onComplete(update -> {

          if (update.failed()) {

            Logger.trace(update.cause(), "Cannot update the task {} with the declined user {}", () -> env.task.id,
                () -> volunteerId);

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
      final var msg = this.createTextualMessage(env, "Deadline reached", "It's too late to refuse to be a volunteer.");
      msg.receiverId = volunteerId;
      env.sendTo(msg);
    }

  }

  /**
   * Called when an user is accepted to provide help.
   *
   * @param volunteerId identifier of the volunteer that is accepted to provide
   *                    help.
   * @param env         to get the data.
   */
  private void handleAcceptVolunteer(final String volunteerId, final HardCodedProtocolEnvironment env) {

    final var volunteers = env.task.attributes.getJsonArray("volunteers");
    if (!volunteers.remove(volunteerId)) {

      final var msg = this.createTextualMessage(env, "Unexpected volunteer to accept",
          "The user '" + volunteerId + "' is not a volunteer of the task, so you can not accept it.");
      msg.receiverId = env.task.requesterId;
      env.sendTo(msg);

    } else {

      final var accepted = env.task.attributes.getJsonArray("accepted");
      accepted.add(volunteerId);
      final var taskWhereAccepted = new Task();
      taskWhereAccepted.attributes = env.task.attributes;
      WeNetTaskManager.createProxy(this.vertx).mergeTask(env.task.id, taskWhereAccepted).onComplete(update -> {

        if (update.failed()) {

          Logger.trace(update.cause(), "Cannot update the task {} with the accepted user {}", () -> env.task.id,
              () -> volunteerId);

        } else {

          Logger.trace("Added accepted users {} into task {}", () -> volunteerId, () -> env.task.id);
          final var notification = this.createMessageWithCommunityandTaskIds(env);
          notification.label = "TaskSelectionNotification";
          notification.receiverId = volunteerId;
          notification.attributes.put("outcome", "accepted");
          env.sendTo(notification);

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
   * @param volunteerId identifier of the volunteer that is refused to provide
   *                    help.
   * @param env         to get the data.
   */
  private void handleRefuseVolunteer(final String volunteerId, final HardCodedProtocolEnvironment env) {

    final var volunteers = env.task.attributes.getJsonArray("volunteers");
    if (!volunteers.remove(volunteerId)) {

      final var msg = this.createTextualMessage(env, "Unexpected volunteer to refuse",
          "The user '" + volunteerId + "' is not a volunteer of the task, so you can not refuse it.");
      msg.receiverId = env.task.requesterId;
      env.sendTo(msg);

    } else {

      final var refused = env.task.attributes.getJsonArray("refused");
      refused.add(volunteerId);
      final var taskWhereRefused = new Task();
      taskWhereRefused.attributes = env.task.attributes;
      WeNetTaskManager.createProxy(this.vertx).mergeTask(env.task.id, taskWhereRefused).onComplete(update -> {

        if (update.failed()) {

          Logger.trace(update.cause(), "Cannot update the task {} with the refused user {}", () -> env.task.id,
              () -> volunteerId);

        } else {

          Logger.trace("Added refused users {} into task {}", () -> volunteerId, () -> env.task.id);
          final var notification = this.createMessageWithCommunityandTaskIds(env);
          notification.label = "TaskSelectionNotification";
          notification.receiverId = volunteerId;
          notification.attributes.put("outcome", "refused");
          env.sendTo(notification);

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
   */
  private void handleTaskCompleted(final String outcome, final HardCodedProtocolEnvironment env) {

    final var closedTask = new Task();
    closedTask.closeTs = TimeManager.now();
    closedTask.attributes = env.task.attributes;
    closedTask.attributes.put("outcome", outcome);
    WeNetTaskManager.createProxy(this.vertx).mergeTask(env.task.id, closedTask).onComplete(update -> {

      if (update.failed()) {

        Logger.trace("Cannot mark the task {} as closed", () -> env.task.id);

      } else {

        Logger.trace("Closed {} task", () -> env.task.id);

        final var notification = this.createMessageWithCommunityandTaskIds(env);
        notification.label = "TaskConcludedNotification";
        notification.attributes.put("outcome", outcome);
        final var unanswered = env.task.attributes.getJsonArray("unanswered");
        env.sendTo(unanswered, notification);
        final var volunteers = env.task.attributes.getJsonArray("volunteers");
        env.sendTo(volunteers, notification);
        final var accepted = env.task.attributes.getJsonArray("accepted");
        env.sendTo(accepted, notification);

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
   * @param incentive to send.
   */
  protected void handleIncentive(final Incentive incentive) {

    WeNetService.createProxy(this.vertx).retrieveApp(incentive.AppID).onComplete(retrieveApp -> {

      if (retrieveApp.failed()) {

        Logger.trace(retrieveApp.cause(), "No found APP to inform of the incentive {}", incentive);

      } else {

        WeNetService.createProxy(this.vertx).retrieveAppUserIds(incentive.AppID, retrieveUsers -> {

          if (retrieveUsers.failed()) {

            Logger.trace(retrieveUsers.cause(), "No found users to inform of the incentive {}", incentive);

          } else {

            final var app = retrieveApp.result();
            final var appUsers = retrieveUsers.result();
            final var options = new WebClientOptions();
            final var client = WebClient.create(this.vertx, options);

            final var notification = new Message();
            notification.appId = incentive.AppID;
            notification.receiverId = incentive.UserId;
            notification.attributes = new JsonObject().put("issuer", incentive.Issuer);
            if (incentive.Message != null) {

              notification.attributes.put("content", incentive.Message.content);

            } else {

              notification.attributes.put("content", incentive.Badge.Message);

            }
            final var max = appUsers.size();
            for (int i = 0; i < max; i++) {

              notification.receiverId = appUsers.getString(i);
              final var body = notification.toJsonObject();
              client.postAbs(app.messageCallbackUrl).sendJsonObject(body, send -> {

                if (send.failed()) {

                  Logger.trace(send.cause(), "App[{}]: POST {} with {} failed", () -> app.appId,
                      () -> app.messageCallbackUrl, () -> body);

                } else {

                  final var response = send.result();
                  Logger.trace("App[{}]: POST {} with {} responds with code {} and body {}", () -> app.appId,
                      () -> app.messageCallbackUrl, () -> body, () -> response.statusCode(),
                      () -> response.bodyAsString());
                }

              });
            }
          }
        });
      }
    });
  }

}
