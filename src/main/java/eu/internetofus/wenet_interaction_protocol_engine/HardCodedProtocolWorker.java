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
     * The transaction that is realizing.
     */
    public TaskTransaction transaction;

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

      if (WeNetTaskManager.HARDCODED_DINNER_TASK_TYPE_ID.equals(task.taskTypeId)) {

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
            if (this.transaction != null) {

              var msg = Model.fromJsonObject(body, Message.class);
              WeNetTaskManager.createProxy(HardCodedProtocolWorker.this.vertx)
                  .addMessageIntoTransaction(this.task.id, this.transaction.id, msg).onComplete(added -> {

                    if (added.failed()) {

                      Logger.trace(added.cause(), "App[{}]: Can not add {} into the transaction  {} of the task {}",
                          this.task.appId, msg, this.transaction.id, this.task.id);

                    } else {

                      Logger.trace("App[{}]: Added {} into the transaction  {} of the task {}", this.task.appId, msg,
                          this.transaction.id, this.task.id);
                    }

                  });

            }
          }

        });
      }
    }

    /**
     * Set the transaction to use on the environment.
     *
     * @param transaction to add.
     *
     * @return the future added transaction.
     */
    public Future<Void> addTransaction(TaskTransaction transaction) {

      Promise<Void> promise = Promise.promise();
      transaction.taskId = this.task.id;
      WeNetTaskManager.createProxy(HardCodedProtocolWorker.this.vertx).addTransactionIntoTask(this.task.id, transaction)
          .onComplete(added -> {

            if (added.failed()) {

              Logger.trace(added.cause(), "App[{}]: Can not add {} into the task {}", this.task.appId, transaction,
                  this.task.id);

            } else {

              this.transaction = added.result();
            }
            promise.complete();

          });

      return promise.future();

    }

    /**
     * Update the task.
     *
     * @return the future update transaction.
     */
    public Future<Void> updateTask() {

      Promise<Void> promise = Promise.promise();
      this.transaction.taskId = this.task.id;
      WeNetTaskManager.createProxy(HardCodedProtocolWorker.this.vertx).mergeTask(this.task.id, this.task)
          .onComplete(merged -> {

            if (merged.failed()) {

              Logger.trace(merged.cause(), "App[{}]: Can not update {}", this.task.appId, this.task);

            } else {

              this.task = merged.result();
            }
            promise.complete();

          });

      return promise.future();

    }

    /**
     * Return the identifiers of the app users except the requester.
     *
     * @return the application users.
     */
    protected Future<JsonArray> getAppUser() {

      Promise<JsonArray> promise = Promise.promise();
      WeNetService.createProxy(HardCodedProtocolWorker.this.vertx).retrieveAppUserIds(this.task.appId, retrieve -> {

        if (retrieve.failed()) {

          Logger.trace(retrieve.cause(), "No found users from {} to inform of the created task {}",
              () -> this.task.appId, () -> this.task.id);
          promise.complete(new JsonArray());

        } else {

          final var appUsers = retrieve.result();
          promise.complete(appUsers);
        }
      });
      return promise.future();
    }

    /**
     * Return the identifiers of the app users except the requester.
     *
     * @return the application users.
     */
    protected Future<JsonArray> getAppUserExceptRequester() {

      return this.getAppUser().compose(users -> {
        users.remove(this.task.requesterId);
        return Future.succeededFuture(users);
      });

    }

  } // End Class 'HardCodedProtocolEnvironment'

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
  protected Message createMessageWithTaskId(HardCodedProtocolEnvironment env) {

    var msg = this.createMessage(env);
    msg.attributes.put("taskId", env.task.id);
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

    var msg = this.createMessageWithTaskId(env);
    msg.appId = env.task.appId;
    msg.attributes.put("communityId", env.task.communityId);
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

    if (WeNetTaskManager.QUESTION_AND_ANSWER_TASK_TYPE_ID.equals(env.task.taskTypeId)) {

      this.handleQuestionAndAnswerTaskTransaction(env, transaction);

    } else {

      this.handleEatTaskTransaction(env, transaction);
    }

  }

  /**
   * Handle a task transaction over the eat protocol.
   *
   * @param env         protocol environment.
   * @param transaction to handle.
   */
  protected void handleEatTaskTransaction(final HardCodedProtocolEnvironment env, final TaskTransaction transaction) {

    if (env.task.closeTs == null) {

      final JsonObject attr = transaction.attributes;
      if ("volunteerForTask".equalsIgnoreCase(transaction.label)) {

        final var volunteerId = attr.getString("volunteerId", "0");
        this.handleVolunteerForTask(volunteerId, env, transaction);

      } else if ("refuseTask".equalsIgnoreCase(transaction.label)) {

        final var volunteerId = attr.getString("volunteerId", "0");
        this.handleRefuseTask(volunteerId, env, transaction);

      } else if ("acceptVolunteer".equalsIgnoreCase(transaction.label)) {

        final var volunteerId = attr.getString("volunteerId", "0");
        this.handleAcceptVolunteer(volunteerId, env, transaction);

      } else if ("refuseVolunteer".equalsIgnoreCase(transaction.label)) {

        final var volunteerId = attr.getString("volunteerId", "0");
        this.handleRefuseVolunteer(volunteerId, env, transaction);

      } else if ("taskCompleted".equalsIgnoreCase(transaction.label)) {

        final var outcome = attr.getString("outcome", "cancelled");
        this.handleTaskCompleted(outcome, env, transaction);

      } else {

        Logger.trace("Unexpected transaction {}", transaction);
        final var msg = this.createTextualMessage(env, "Undefined transaction",
            "The transaction that you try to do is not allowed on this protocol.");
        msg.receiverId = transaction.actioneerId;
        env.sendTo(msg);
      }

    } else {
      // Error task closed
      final var msg = this.createTextualMessage(env, "Task already closed",
          "It's too late the task is already completed.");
      msg.receiverId = transaction.actioneerId;
      env.sendTo(msg);
    }

  }

  /**
   * Handle the message that inform that a task has been created.
   *
   * @param env protocol environment.
   */
  private void handleTaskCreated(final HardCodedProtocolEnvironment env) {

    if (WeNetTaskManager.HARDCODED_DINNER_TASK_TYPE_ID.equals(env.task.taskTypeId)) {

      this.createdEatProtocol(env);

    } else {

      this.createdQuestionAndAnswerProtocol(env);
    }

  }

  /**
   * Build a new transaction for the created task.
   *
   * @param env to use.
   *
   * @return the create task transaction.
   */
  private TaskTransaction buildCreateTaskTransaction(HardCodedProtocolEnvironment env) {

    var transaction = new TaskTransaction();
    transaction.taskId = env.task.id;
    transaction.actioneerId = env.task.requesterId;
    transaction.label = "CREATE_TASK";
    return transaction;

  }

  /**
   * Handle the created eat protocol.
   *
   * @param env protocol environment.
   */
  private void createdEatProtocol(final HardCodedProtocolEnvironment env) {

    if (env.task.attributes == null || env.task.attributes.getLong("deadlineTs") == null
        || env.task.attributes.getLong("deadlineTs") <= TimeManager.now()) {

      var notification = this.createTextualMessage(env, "Bad deadlineTs",
          "A new task require a deadline time-stamp that has to be greater than now.");
      notification.receiverId = env.task.requesterId;
      env.sendTo(notification);

    } else {

      var transaction = this.buildCreateTaskTransaction(env);
      env.addTransaction(transaction).compose(added -> env.getAppUserExceptRequester()).onSuccess(appUsers -> {
        appUsers.remove(env.task.requesterId);
        // TO DO RANKING users before ask them
        env.task.attributes.put("unanswered", appUsers);
        env.updateTask().onComplete(update -> {

          final var notification = this.createMessageWithCommunityandTaskIds(env);
          notification.label = "TaskProposalNotification";
          env.sendTo(appUsers, notification);
          this.notifyIncentiveServerTaskCreated(env);

        });
      });
    }
  }

  /**
   * Called when an user has offered as volunteer.
   *
   * @param volunteerId identifier of the user that has offer its help.
   * @param env         to get the data.
   * @param transaction to do.
   */
  private void handleVolunteerForTask(final String volunteerId, final HardCodedProtocolEnvironment env,
      final TaskTransaction transaction) {

    var deadlineTs = env.task.attributes.getLong("deadlineTs", 0l);
    if (deadlineTs > TimeManager.now()) {

      final var unanswered = env.task.attributes.getJsonArray("unanswered");
      if (!unanswered.remove(volunteerId)) {

        final var msg = this.createTextualMessage(env, "Accept not allowed",
            "You cannot be a volunteer, because you already are or you are not a person that can provide help.");
        msg.receiverId = volunteerId;
        env.sendTo(msg);

      } else {

        env.addTransaction(transaction).onComplete(added -> {

          var volunteers = env.task.attributes.getJsonArray("volunteers");
          volunteers.add(volunteerId);
          env.updateTask().onComplete(update -> {

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

          });
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
   * Notify the incentive server that the task is created.
   *
   * @param env to use.
   */
  private void notifyIncentiveServerTaskCreated(HardCodedProtocolEnvironment env) {

    final var status = new TaskStatus();
    status.user_id = env.task.requesterId;
    status.task_id = env.task.id;
    status.Action = "taskCreated";
    status.Message = "A task is created";
    this.notifyIncentiveServerTaskStatusChanged(status);

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
   * @param transaction to do.
   */
  private void handleRefuseTask(final String volunteerId, final HardCodedProtocolEnvironment env,
      final TaskTransaction transaction) {

    var deadlineTs = env.task.attributes.getLong("deadlineTs", null);
    if (deadlineTs != null && deadlineTs > TimeManager.now()) {

      final var unanswered = env.task.attributes.getJsonArray("unanswered");
      if (!unanswered.remove(volunteerId)) {

        final var msg = this.createTextualMessage(env, "Refuse not allowed",
            "You cannot refuse to be a volunteer, because you already refused or you are not a person that can provide help.");
        msg.receiverId = volunteerId;
        env.sendTo(msg);

      } else {

        env.addTransaction(transaction).onComplete(added -> {
          final var declined = env.task.attributes.getJsonArray("declined");
          declined.add(volunteerId);
          env.updateTask().onComplete(update -> {

            Logger.trace("Added declined users {} into task {}", () -> volunteerId, () -> env.task.id);
            final var status = new TaskStatus();
            status.user_id = volunteerId;
            status.task_id = env.task.id;
            status.Action = "refuseTask";
            status.Message = "An user has declined to be a volunteer for a task.";
            this.notifyIncentiveServerTaskStatusChanged(status);

          });
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
   * @param transaction to do.
   */
  private void handleAcceptVolunteer(final String volunteerId, final HardCodedProtocolEnvironment env,
      final TaskTransaction transaction) {

    final var volunteers = env.task.attributes.getJsonArray("volunteers");
    if (!volunteers.remove(volunteerId)) {

      final var msg = this.createTextualMessage(env, "Unexpected volunteer to accept",
          "The user '" + volunteerId + "' is not a volunteer of the task, so you can not accept it.");
      msg.receiverId = env.task.requesterId;
      env.sendTo(msg);

    } else {

      env.addTransaction(transaction).onComplete(added -> {

        final var accepted = env.task.attributes.getJsonArray("accepted");
        accepted.add(volunteerId);
        env.updateTask().onComplete(update -> {

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

        });
      });
    }
  }

  /**
   * Called when an user is refused to provide help.
   *
   * @param volunteerId identifier of the volunteer that is refused to provide
   *                    help.
   * @param env         to get the data.
   * @param transaction to do.
   */
  private void handleRefuseVolunteer(final String volunteerId, final HardCodedProtocolEnvironment env,
      final TaskTransaction transaction) {

    final var volunteers = env.task.attributes.getJsonArray("volunteers");
    if (!volunteers.remove(volunteerId)) {

      final var msg = this.createTextualMessage(env, "Unexpected volunteer to refuse",
          "The user '" + volunteerId + "' is not a volunteer of the task, so you can not refuse it.");
      msg.receiverId = env.task.requesterId;
      env.sendTo(msg);

    } else {

      env.addTransaction(transaction).onComplete(added -> {
        final var refused = env.task.attributes.getJsonArray("refused");
        refused.add(volunteerId);
        env.updateTask().onComplete(update -> {

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

        });
      });

    }

  }

  /**
   * Called when an user mark a task as completed.
   *
   * @param outcome     state of the completed task.
   * @param env         to get the data.
   * @param transaction to do.
   */
  private void handleTaskCompleted(final String outcome, final HardCodedProtocolEnvironment env,
      final TaskTransaction transaction) {

    env.addTransaction(transaction).onComplete(added -> {
      env.task.closeTs = TimeManager.now();
      env.task.attributes.put("outcome", outcome);
      env.updateTask().onComplete(update -> {

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

      });
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

        final var app = retrieveApp.result();
        final var options = new WebClientOptions();
        final var client = WebClient.create(this.vertx, options);

        final var notification = new Message();
        notification.appId = incentive.AppID;
        notification.receiverId = incentive.UserId;
        notification.attributes = new JsonObject().put("issuer", incentive.Issuer);
        if (incentive.Message != null) {

          notification.label = "IncentiveMessage";
          notification.attributes.put("content", incentive.Message.content);

        } else {

          notification.label = "IncentiveBadge";
          notification.attributes.put("badgeClass", incentive.Badge.BadgeClass);
          notification.attributes.put("imageUrl", incentive.Badge.ImgUrl);
          notification.attributes.put("criteria", incentive.Badge.Criteria);
          notification.attributes.put("message", incentive.Badge.Message);

        }

        final var body = notification.toJsonObject();
        client.postAbs(app.messageCallbackUrl).sendJsonObject(body, send -> {

          if (send.failed()) {

            Logger.trace(send.cause(), "App[{}]: POST {} with {} failed", () -> app.appId, () -> app.messageCallbackUrl,
                () -> body);

          } else {

            final var response = send.result();
            Logger.trace("App[{}]: POST {} with {} responds with code {} and body {}", () -> app.appId,
                () -> app.messageCallbackUrl, () -> body, () -> response.statusCode(), () -> response.bodyAsString());
          }

        });

      }
    });
  }

  /**
   * Handle the created question and answers protocol.
   *
   * @param env protocol environment.
   */
  private void createdQuestionAndAnswerProtocol(HardCodedProtocolEnvironment env) {

    var transaction = this.buildCreateTaskTransaction(env);
    env.addTransaction(transaction).compose(added -> env.getAppUserExceptRequester()).onSuccess(appUsers -> {

      final var msg = this.createMessageWithTaskId(env);
      msg.label = "QuestionToAnswerMessage";
      msg.receiverId = env.task.requesterId;
      msg.attributes.put("question", env.task.goal.name).put("userId", env.task.requesterId);

      env.sendTo(appUsers, msg);

      this.notifyIncentiveServerTaskCreated(env);

    });

  }

  /**
   * Handle a task transaction over the quesitons and answers protocol.
   *
   * @param env         protocol environment.
   * @param transaction to handle.
   */
  protected void handleQuestionAndAnswerTaskTransaction(final HardCodedProtocolEnvironment env,
      final TaskTransaction transaction) {

    if (env.task.closeTs == null) {

      final JsonObject attr = transaction.attributes;
      if ("answerTransaction".equalsIgnoreCase(transaction.label)) {

        final var answer = attr.getString("answer");
        this.handleAnswerTransaction(answer, env, transaction);

      } else if ("notAnswerTransaction".equalsIgnoreCase(transaction.label)) {

        this.handleNotAnswerTransaction(env, transaction);

      } else if ("bestAnswerTransaction".equalsIgnoreCase(transaction.label)) {

        final var transactionId = attr.getString("transactionId");
        final var reason = attr.getString("reason");
        this.handleBestAnswerTransaction(transactionId, reason, env, transaction);

      } else if ("moreAnswerTransaction".equalsIgnoreCase(transaction.label)) {

        this.handleMoreAnswerTransaction(env, transaction);

      } else if ("reportQuestionTransaction".equalsIgnoreCase(transaction.label)) {

        final var reason = attr.getString("reason");
        final var comment = attr.getString("comment");
        this.handleReportQuestionTransaction(reason, comment, env, transaction);

      } else if ("reportAnswerTransaction".equalsIgnoreCase(transaction.label)) {

        final var transactionId = attr.getString("transactionId");
        final var reason = attr.getString("reason");
        final var comment = attr.getString("comment");
        this.handleReportAnswerTransaction(transactionId, reason, comment, env, transaction);

      } else {

        Logger.trace("Unexpected {} over the Q&A protocol", transaction);
      }

    }

  }

  /**
   * Answer to a question.
   *
   * @param answer      to the question
   * @param env         to get the data.
   * @param transaction to do.
   */
  private void handleAnswerTransaction(String answer, HardCodedProtocolEnvironment env, TaskTransaction transaction) {

    env.addTransaction(transaction).onComplete(added -> {

      final var msg = this.createMessageWithTaskId(env);
      msg.label = "AnsweredQuestionMessage";
      msg.receiverId = env.task.requesterId;
      msg.attributes.put("answer", answer).put("transactionId", env.transaction.id).put("userId",
          env.transaction.actioneerId);
      env.sendTo(msg);

    });

  }

  /**
   * Ignore a question.
   *
   * @param env         to get the data.
   * @param transaction to do.
   */
  private void handleNotAnswerTransaction(HardCodedProtocolEnvironment env, TaskTransaction transaction) {

    env.addTransaction(transaction);

  }

  /**
   * Check if an answer transaction is done on the task.
   *
   * @param transactionId the id of the picked answer transaction
   * @param env           to get the data.
   *
   * @return {@code true} if exist the transaction.
   */
  private boolean checkAnswerTransactionDone(String transactionId, HardCodedProtocolEnvironment env) {

    for (var doneTranssaction : env.task.transactions) {

      if (doneTranssaction.id.equals(transactionId) && "answerTransaction".equals(doneTranssaction.label)) {

        return true;
      }

    }

    return false;
  }

  /**
   * Pick the best answer.
   *
   * @param transactionId the id of the picked answer transaction
   * @param reason        the reason why the specific answer was picked
   * @param env           to get the data.
   * @param transaction   to do.
   */
  private void handleBestAnswerTransaction(String transactionId, String reason, HardCodedProtocolEnvironment env,
      TaskTransaction transaction) {

    if (this.checkAnswerTransactionDone(transactionId, env)) {

      env.addTransaction(transaction).compose(empty -> {

        env.task.closeTs = TimeManager.now();
        return env.updateTask();
      }).onComplete(empty -> {

        Logger.trace("Closed task {} because selected {}, because {}", env.task.id, transactionId, reason);

      });

    } else {

      Logger.trace("No found transaction {} to select best answer into task {}", () -> transactionId,
          () -> env.task.id);

    }
  }

  /**
   * Ask some more users.
   *
   * @param env         to get the data.
   * @param transaction to do.
   */
  private void handleMoreAnswerTransaction(HardCodedProtocolEnvironment env, TaskTransaction transaction) {

    env.addTransaction(transaction);
  }

  /**
   * Report question.
   *
   * @param reason      why the specific answer was picked.
   * @param comment     a specific comment by the reporting user.
   * @param env         to get the data.
   * @param transaction to do.
   */
  private void handleReportQuestionTransaction(String reason, String comment, HardCodedProtocolEnvironment env,
      TaskTransaction transaction) {

    env.addTransaction(transaction).onSuccess(updated -> {

      Logger.trace("Report: {} ({})", reason, comment);

    });

  }

  /**
   * Answer to a question.
   *
   * @param transactionId the id of the picked answer transaction.
   * @param reason        why the specific answer was picked.
   * @param comment       a specific comment by the reporting user.
   * @param env           to get the data.
   * @param transaction   to do.
   */
  private void handleReportAnswerTransaction(String transactionId, String reason, String comment,
      HardCodedProtocolEnvironment env, TaskTransaction transaction) {

    if (this.checkAnswerTransactionDone(transactionId, env)) {

      env.addTransaction(transaction).onSuccess(updated -> {

        Logger.trace("Report[]: {} ({})", transactionId, reason, comment);

      });

    } else {

      Logger.trace("No found transaction {} to report the answer into task {}", () -> transactionId, () -> env.task.id);

    }

  }

}
