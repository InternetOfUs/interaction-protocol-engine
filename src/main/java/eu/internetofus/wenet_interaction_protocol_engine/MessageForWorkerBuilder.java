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

import eu.internetofus.common.components.incentive_server.Incentive;
import eu.internetofus.common.components.interaction_protocol_engine.ProtocolAddress;
import eu.internetofus.common.components.interaction_protocol_engine.ProtocolAddress.Component;
import eu.internetofus.common.components.interaction_protocol_engine.ProtocolMessage;
import eu.internetofus.common.components.task_manager.Task;
import eu.internetofus.common.components.task_manager.TaskTransaction;
import io.vertx.core.json.JsonObject;

/**
 * Component used to create the messages that can be wend to a worker.
 *
 * @see HardCodedProtocolWorker
 * @see EngineWorker
 *
 * @author UDT-IA, IIIA-CSIC
 */
public interface MessageForWorkerBuilder {

  /**
   * The types of messages that the worker can receive.
   */
  public enum Type {
    /**
     * When the message inform that a task is created.
     */
    CREATED_TASK,

    /**
     * When the message inform about a task transaction that has to be done.
     */
    DO_TASK_TRANSACTION,

    /**
     * When the message inform about an incentive to inform to the user.
     */
    SEND_INCENTIVE,

    /**
     * When the message refers to the interaction protocol.
     */
    PROTOCOL_MESSAGE;

  }

  /**
   * Create the message to inform that a task is created.
   *
   * @param task that has been created.
   *
   * @return the message to send to the worker.
   */
  public static JsonObject buildCreatedTaskMessage(final Task task) {

    return new JsonObject().put("type", Type.CREATED_TASK.name()).put("task", task.toJsonObject());

  }

  /**
   * Create the message to inform that a task is created.
   *
   * @param protocol information of the protocol to use.
   *
   * @return the message to send to the worker.
   */
  public static JsonObject buildCreatedTaskMessage(final ProtocolData protocol) {

    return new JsonObject().put("type", Type.CREATED_TASK.name()).put("protocol", protocol.toJsonObject());

  }

  /**
   * Create the message to inform about a task transaction that has to be done.
   *
   * @param transaction to do.
   *
   * @return the message to send to the worker.
   */
  public static JsonObject buildDoTaskTransactionMessage(final TaskTransaction transaction) {

    return new JsonObject().put("type", Type.DO_TASK_TRANSACTION.name()).put("transaction", transaction.toJsonObject());

  }

  /**
   * Create the message to inform about a task transaction that has to be done.
   *
   * @param transaction to do.
   * @param protocol    information of the protocol to use.
   *
   * @return the message to send to the worker.
   */
  public static JsonObject buildDoTaskTransactionMessage(final TaskTransaction transaction,
      final ProtocolData protocol) {

    return new JsonObject().put("type", Type.DO_TASK_TRANSACTION.name()).put("transaction", transaction.toJsonObject())
        .put("protocol", protocol.toJsonObject());

  }

  /**
   * Create the message to inform about an incentive to inform to the user.
   *
   * @param incentive to send.
   *
   * @return the message to send to the worker.
   */
  public static JsonObject buildSendIncentiveMessage(final Incentive incentive) {

    return new JsonObject().put("type", Type.SEND_INCENTIVE.name()).put("incentive", incentive.toJsonObject());

  }

  /**
   * Create the message to inform about an incentive to inform to the user.
   *
   * @param incentive to send.
   * @param protocol  information of the protocol to use.
   *
   * @return the message to send to the worker.
   */
  public static JsonObject buildSendIncentiveMessage(final Incentive incentive, final ProtocolData protocol) {

    return new JsonObject().put("type", Type.SEND_INCENTIVE.name()).put("incentive", incentive.toJsonObject())
        .put("protocol", protocol.toJsonObject());

  }

  /**
   * Create the message for the worker with the protocol message to process.
   *
   * @param message  to send.
   * @param protocol information of the protocol to use.
   *
   * @return the message to send to the worker.
   */
  public static JsonObject buildProtocolMessage(final ProtocolMessage message, final ProtocolData protocol) {

    return new JsonObject().put("type", Type.PROTOCOL_MESSAGE.name()).put("message", message.toJsonObject())
        .put("protocol", protocol.toJsonObject());

  }

  /**
   * Create the protocol message for a protocol.
   *
   * @param protocol to create the message.
   *
   * @return the message associated to the protocol.
   */
  public static ProtocolMessage createProtocolMessage(final ProtocolData protocol) {

    final var message = new ProtocolMessage();
    if (protocol.task != null) {

      message.appId = protocol.task.appId;
      message.communityId = protocol.task.communityId;
      message.taskId = protocol.task.id;

    } else if (protocol.community != null) {

      message.communityId = protocol.community.id;
      message.appId = protocol.community.appId;
    }
    message.sender = new ProtocolAddress();
    message.receiver = new ProtocolAddress();
    message.receiver.component = Component.INTERACTION_PROTOCOL_ENGINE;
    if (protocol.profile != null) {

      message.receiver.userId = protocol.profile.id;
    }
    return message;

  }

  /**
   * Create the message for the worker with the protocol message to process an
   * incentive to send.
   *
   * @param incentive to send.
   * @param protocol  information of the protocol to use.
   *
   * @return the message to send to the worker.
   */
  public static JsonObject buildProtocolMessageForSendIncentive(final Incentive incentive,
      final ProtocolData protocol) {

    final var message = createProtocolMessage(protocol);
    message.appId = incentive.AppID;
    message.sender.component = Component.INCENTIVE_SERVER;
    message.receiver.userId = incentive.UserId;
    message.particle = "sendIncentive";
    message.content = incentive.toJsonObject();
    return buildProtocolMessage(message, protocol);

  }

  /**
   * Create the message for the worker with the protocol message to process a
   * created task event.
   *
   * @param protocol information of the protocol to use.
   *
   * @return the message to send to the worker.
   */
  public static JsonObject buildProtocolMessageForCreatedTask(final ProtocolData protocol) {

    final var message = createProtocolMessage(protocol);
    message.sender.component = Component.USER_APP;
    message.sender.userId = message.receiver.userId = protocol.task.requesterId;
    message.particle = "createdTask";
    return buildProtocolMessage(message, protocol);

  }

  /**
   * Create the message for the worker with the protocol message to process a
   * transaction to do.
   *
   * @param transaction to do.
   * @param protocol    information of the protocol to use.
   *
   * @return the message to send to the worker.
   */
  public static JsonObject buildProtocolMessageForDoTaskTransaction(final TaskTransaction transaction,
      final ProtocolData protocol) {

    final var message = createProtocolMessage(protocol);
    message.sender.component = Component.USER_APP;
    message.sender.userId = message.receiver.userId = transaction.actioneerId;
    message.particle = "doTaskTransaction";
    message.content = transaction.toJsonObject();
    return buildProtocolMessage(message, protocol);

  }

}
